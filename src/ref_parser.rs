use crate::{
    utils, ByteSequence, Decimal, FromPrimitive, FromStr, Num, Parser, RefBareItem, SFVResult,
};
use std::iter::Peekable;
use std::str::{CharIndices, Chars};

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    RefBareItem(RefBareItem<'a>),
    Parameter {
        key: &'a str,
        value: RefBareItem<'a>,
    },
}

#[derive(Debug, PartialEq)]
enum State {
    Start,
    BareItem,
    Parameter,
    Finish,
}

pub struct ItemTokenizer<'a> {
    tokenizer: Tokenizer<'a>,
    state: State,
}

impl<'a> ItemTokenizer<'a> {
    pub fn new(input: &'a str) -> SFVResult<Self> {
        if !input.is_ascii() {
            return Err("parse: non-ascii characters in input");
        }

        Ok(Self {
            tokenizer: Tokenizer::new(&input),
            state: State::Start,
        })
    }

    pub fn next(&mut self) -> SFVResult<Token<'a>> {
        if self.state == State::Start {
            self.tokenizer.consume_sp_chars();
            self.state = State::BareItem;
        }

        match self.state {
            State::BareItem => {
                let result = self.tokenizer.parse_bare_item()?;
                self.state = State::Parameter;
                Ok(Token::RefBareItem(result))
            }
            // State::Parameter => self.parse_parameter()?,
            _ => return Err("parse_item: invalid state"),
        }
    }
}

#[derive(Debug)]
struct Tokenizer<'a> {
    input: &'a str,
    offset: usize,
    chars: Peekable<Chars<'a>>,
}

impl<'a> Tokenizer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            offset: 0,
            chars: input.chars().peekable(),
        }
    }

    fn consume_sp_chars(&mut self) {
        while let Some(ch) = self.chars.peek() {
            if ch == &' ' {
                self.chars.next();
            } else {
                break;
            }
        }
    }

    fn consume_ows_chars(&mut self) {
        while let Some(c) = self.chars.peek() {
            if c == &' ' || c == &'\t' {
                self.chars.next();
            } else {
                break;
            }
        }
    }

    fn take_char(&mut self) -> Option<char> {
        let ch = self.chars.next();
        if ch.is_some() {
            self.offset += 1; // as tokenizer works with ASCII only chars only
        }
        ch
    }

    fn parse_bare_item(&mut self) -> SFVResult<RefBareItem<'a>> {
        // https://httpwg.org/http-extensions/draft-ietf-httpbis-header-structure.html#parse-bare-item

        if self.chars.peek().is_none() {
            return Err("parse_bare_item: empty item");
        }

        match self.chars.peek() {
            Some('?') => Ok(RefBareItem::Boolean(self.parse_bool()?)),
            Some('"') => Ok(RefBareItem::String(self.parse_string()?)),
            Some(':') => Ok(RefBareItem::ByteSeq(self.parse_byte_sequence()?)),
            Some(&ch) if ch == '*' || ch.is_ascii_alphabetic() => {
                Ok(RefBareItem::Token(self.parse_token()?))
            }
            Some(&ch) if ch == '-' || ch.is_ascii_digit() => match self.parse_number()? {
                Num::Decimal(val) => Ok(RefBareItem::Decimal(val)),
                Num::Integer(val) => Ok(RefBareItem::Integer(val)),
            },
            _ => Err("parse_bare_item: item type can't be identified"),
        }
    }

    fn parse_bool(&mut self) -> SFVResult<bool> {
        // https://httpwg.org/http-extensions/draft-ietf-httpbis-header-structure.html#parse-boolean

        if self.take_char() != Some('?') {
            return Err("parse_bool: first character is not '?'");
        }

        match self.take_char() {
            Some('0') => Ok(false),
            Some('1') => Ok(true),
            _ => Err("parse_bool: invalid variant"),
        }
    }

    fn parse_string(&mut self) -> SFVResult<&'a str> {
        // https://httpwg.org/http-extensions/draft-ietf-httpbis-header-structure.html#parse-string

        if self.take_char() != Some('\"') {
            return Err("parse_string: first character is not '\"'");
        }

        let start_idx = self.offset;
        while let Some(ch) = self.take_char() {
            match ch {
                '\"' => return Ok(&self.input[start_idx..(self.offset - ch.len_utf8())]), // do not include closing quote into string value
                '\x7f' | '\x00'..='\x1f' => return Err("parse_string: not a visible character"),
                '\\' => match self.take_char() {
                    Some(c) if c == '\\' || c == '\"' => {
                        continue;
                    }
                    None => return Err("parse_string: last input character is '\\'"),
                    _ => return Err("parse_string: disallowed character after '\\'"),
                },
                _ => continue,
            }
        }
        Err("parse_string: no closing '\"'")
    }

    fn parse_token(&mut self) -> SFVResult<&'a str> {
        // https://httpwg.org/http-extensions/draft-ietf-httpbis-header-structure.html#parse-token

        if let Some(first_char) = self.chars.peek() {
            if !first_char.is_ascii_alphabetic() && first_char != &'*' {
                return Err("parse_token: first character is not ALPHA or '*'");
            }
        } else {
            return Err("parse_token: empty input string");
        }

        let start_idx = self.offset;
        while let Some(ch) = self.chars.peek() {
            if !utils::is_tchar(*ch) && ch != &':' && ch != &'/' {
                return Ok(&self.input[start_idx..self.offset]);
            }

            if self.take_char().is_none() {
                return Err("parse_token: end of the string");
            }
        }
        return Ok(&self.input[start_idx..self.offset]);
    }

    fn parse_byte_sequence(&mut self) -> SFVResult<ByteSequence<'a>> {
        // https://httpwg.org/http-extensions/draft-ietf-httpbis-header-structure.html#parse-binary

        if self.take_char() != Some(':') {
            return Err("parse_byte_seq: first char is not ':'");
        }

        if !self.chars.clone().any(|c| c == ':') {
            return Err("parse_byte_seq: no closing ':'");
        }

        let start_idx = self.offset;
        while let Some(ch) = self.take_char() {
            if ch == ':' {
                break;
            }
        }

        let byte_seq = &self.input[start_idx..(self.offset - 1)];
        if !byte_seq.chars().all(utils::is_allowed_b64_content) {
            return Err("parse_byte_seq: invalid char in byte sequence");
        }

        Ok(ByteSequence::new(byte_seq.as_ref(), true))
    }

    fn parse_number(&mut self) -> SFVResult<Num> {
        // https://httpwg.org/http-extensions/draft-ietf-httpbis-header-structure.html#parse-number

        let mut sign = 1;
        if let Some('-') = self.chars.peek() {
            sign = -1;
            self.take_char();
        }

        match self.chars.peek() {
            Some(ch) if !ch.is_ascii_digit() => {
                return Err("parse_number: input number does not start with a digit")
            }
            None => return Err("parse_number: input number lacks a digit"),
            _ => (),
        }

        // Get number from input as a string and identify whether it's a decimal or integer
        let (is_integer, input_number) = self.extract_digits()?;

        // Parse input_number from string into integer
        if is_integer {
            let output_number = input_number
                .parse::<i64>()
                .map_err(|_err| "parse_number: parsing i64 failed")?
                * sign;

            let (min_int, max_int) = (-999_999_999_999_999_i64, 999_999_999_999_999_i64);
            if !(min_int <= output_number && output_number <= max_int) {
                return Err("parse_number: integer number is out of range");
            }

            return Ok(Num::Integer(output_number));
        }

        // Parse input_number from string into decimal
        let chars_after_dot = input_number
            .find('.')
            .map(|dot_pos| input_number.len() - dot_pos - 1);

        match chars_after_dot {
            Some(0) => Err("parse_number: decimal ends with '.'"),
            Some(1..=3) => {
                let mut output_number = Decimal::from_str(input_number)
                    .map_err(|_err| "parse_number: parsing f64 failed")?;

                if sign == -1 {
                    output_number.set_sign_negative(true)
                }

                Ok(Num::Decimal(output_number))
            }
            _ => Err("parse_number: invalid decimal fraction length"),
        }
    }

    fn extract_digits(&mut self) -> SFVResult<(bool, &str)> {
        let mut is_integer = true;

        let start_idx = self.offset;
        while let Some(ch) = self.chars.peek() {
            if ch.is_ascii_digit() {
                self.take_char();
            } else if ch == &'.' && is_integer {
                if self.offset - start_idx > 12 {
                    return Err(
                        "parse_number: decimal too long, illegal position for decimal point",
                    );
                }
                is_integer = false;
                self.take_char();
            } else {
                break;
            }

            if is_integer && self.offset - start_idx > 15 {
                return Err("parse_number: integer too long, length > 15");
            }

            if !is_integer && self.offset - start_idx > 16 {
                return Err("parse_number: decimal too long, length > 16");
            }
        }
        Ok((is_integer, &self.input[start_idx..self.offset]))
    }

    // fn parse_param(&mut self) -> SFVResult<Option<(&str, RefBareItem)>> {
    //     // https://httpwg.org/http-extensions/draft-ietf-httpbis-header-structure.html#parse-param
    //
    //     // while let Some(curr_char) = self.chars.peek() {
    //     if self.chars.peek() == &';' {
    //         self.take_char();
    //     } else {
    //         return Ok(None);
    //     }
    //
    //     self.consume_sp_chars();
    //
    //     let param_name = self.parse_key()?;
    //     let param_value = match self.chars.peek() {
    //         Some('=') => {
    //             self.take_char();
    //             self.parse_bare_item()?
    //         }
    //         _ => RefBareItem::Boolean(true),
    //     };
    //
    //     //params.insert(param_name, param_value);
    //
    //     // If parameters already contains a name param_name (comparing character-for-character), overwrite its value.
    //     // Note that when duplicate Parameter keys are encountered, this has the effect of ignoring all but the last instance.
    //     // Ok(params)
    // }

    pub(crate) fn parse_key(&mut self) -> SFVResult<&str> {
        match self.chars.peek() {
            Some(c) if c == &'*' || c.is_ascii_lowercase() => (),
            _ => return Err("parse_key: first character is not lcalpha or '*'"),
        }

        let start_idx = self.offset;
        while let Some(ch) = self.chars.peek() {
            if !ch.is_ascii_lowercase() && !ch.is_ascii_digit() && !"_-*.".contains(*ch) {
                return Ok(&self.input[start_idx..self.offset]);
            }
            self.take_char();
        }
        Ok(&self.input[start_idx..self.offset])
    }
}

#[cfg(test)]
mod tokenizer_tests {
    use super::*;

    #[test]
    fn test_item_tokenizer() -> SFVResult<()> {
        let input = "\"abc fg\"";
        let mut tokenizer = ItemTokenizer::new(input)?;
        assert_eq!(
            tokenizer.next()?,
            Token::RefBareItem(RefBareItem::String("abc fg"))
        );

        let input = "\"\"";
        let mut tokenizer = ItemTokenizer::new(input)?;
        assert_eq!(
            tokenizer.next()?,
            Token::RefBareItem(RefBareItem::String(""))
        );

        let input = "*abcd\\";
        let mut tokenizer = ItemTokenizer::new(input)?;
        assert_eq!(
            tokenizer.next()?,
            Token::RefBareItem(RefBareItem::Token("*abcd"))
        );

        let input = "4586;smth";
        let mut tokenizer = ItemTokenizer::new(input)?;
        assert_eq!(
            tokenizer.next()?,
            Token::RefBareItem(RefBareItem::Integer(4586))
        );

        let input = "4586.56;smth";
        let mut tokenizer = ItemTokenizer::new(input)?;
        assert_eq!(
            tokenizer.next()?,
            Token::RefBareItem(RefBareItem::Decimal(Decimal::from_f64(4586.56).unwrap()))
        );

        let input = ":c3VubnlkYXk=:;smth";
        let mut tokenizer = ItemTokenizer::new(input)?;
        let bare_item = tokenizer.next()?;
        assert_eq!(
            bare_item,
            Token::RefBareItem(RefBareItem::ByteSeq(ByteSequence::new(
                b"c3VubnlkYXk=",
                true
            )))
        );

        match bare_item {
            Token::RefBareItem(RefBareItem::ByteSeq(val)) => {
                assert_eq!(val.decode()?, b"sunnyday");
            }
            _ => return Err("unexpected RefBareItem type"),
        }

        Ok(())
    }
}
