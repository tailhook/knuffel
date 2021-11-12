use std::fmt;
use std::str::CharIndices;

use combine::{StreamOnce, Positioned};
use combine::error::StreamError;

type Error<'a> = combine::easy::Error<Token<'a>, Token<'a>>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum TokenKind {
    String,
    EndOfInput,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Token<'a> {
    pub kind: TokenKind,
    pub value: &'a str,
}

pub(crate) struct TokenStream<'a> {
    buf: &'a str,
    off: usize,
}

impl<'a> TokenStream<'a> {
    pub fn new(data: &'a str) -> TokenStream<'a> {
        let mut result = TokenStream {
            buf: data,
            off: 0,
        };
        result.skip_whitespace();
        return result;
    }
}

impl<'a> TokenStream<'a> {
    fn skip_whitespace(&mut self) {
        let mut iter = self.buf[self.off..].char_indices();
        let idx = 'main: loop {
            let (idx, cur_char) = match iter.next() {
                Some(pair) => pair,
                None => break self.buf.len() - self.off,
            };
            match cur_char {
                ' ' | '\t' => {}
                '\r' | '\n' => break idx,
                //comment
                '/' if iter.as_str().starts_with("/") => {
                    while let Some((_, cur_char)) = iter.next() {
                        if cur_char == '\r' || cur_char == '\n' {
                            break;
                        }
                    }
                    continue;
                }
                '\\' => {
                    while let Some((_, cur_char)) = iter.next() {
                        match cur_char {
                            '\r' | '\n' => continue 'main,
                            ' ' | '\t' => {}
                            '/' => break,
                            // invalid character in continuation
                            // give the tokenizer chance to hanle it
                            _ => break 'main idx,
                        }
                    }
                    if !iter.as_str().starts_with("/") {
                        // invalid character in continuation
                        // give the tokenizer chance to hanle it
                        break idx;
                    }
                    while let Some((_, cur_char)) = iter.next() {
                        if cur_char == '\r' || cur_char == '\n' {
                            break;
                        }
                    }
                }
                _ => break idx,
            }
        };
        self.off += idx;
    }
    fn next_token(&self) -> Result<(TokenKind, usize), Error<'a>> {
        use TokenKind::*;
        let mut iter = self.buf[self.off..].char_indices();
        let cur_char = match iter.next() {
            Some((_, x)) => x,
            None => return Ok((EndOfInput, 0)),
        };
        match cur_char {
            '"' => return self.parse_string(iter).map(|l| (String, l)),
            _ => return Err(Error::unexpected_static_message("character")),
        }
    }
    fn parse_string(&self, iter: CharIndices) -> Result<usize, Error<'a>> {
        let mut escaped = false;
        for (idx, cur_char) in iter {
            match cur_char {
                _ if escaped => escaped = false,
                '"' => return Ok(idx+1),
                '\\' => escaped = true,
                _ => {},
            }
        }
        Err(Error::unexpected_static_message("unterminated quoted string"))
    }
}

impl<'a> StreamOnce for TokenStream<'a> {
    type Token = Token<'a>;
    type Range = Token<'a>;
    type Position = usize;
    type Error = combine::easy::Errors<Token<'a>, Token<'a>, usize>;
    fn uncons(&mut self) -> Result<Token<'a>, Error<'a>> {
        let (kind, len) = self.next_token()?;
        let value = &self.buf[self.off..][..len];
        self.off += len;
        self.skip_whitespace();
        Ok(Token { kind, value })
    }
}

impl<'a> Positioned for TokenStream<'a> {
    fn position(&self) -> usize {
        self.off
    }
}

impl<'a> combine::stream::ResetStream for TokenStream<'a> {
    type Checkpoint = usize;
    fn checkpoint(&self) -> Self::Checkpoint {
        return self.off;
    }
    fn reset(&mut self, checkpoint: usize) -> Result<(), Self::Error> {
        self.off = checkpoint;
        Ok(())
    }
}

impl TokenKind {
    fn describe(&self) -> &'static str {
        use TokenKind::*;

        match self {
            String => "string",
            EndOfInput => "end of input",
        }
    }
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if matches!(&self.kind, TokenKind::EndOfInput) {
            "end of file".fmt(f)
        } else {
            write!(f, "`{}` ({})",
                self.value.escape_default(), self.kind.describe())
        }
    }
}

#[cfg(test)]
mod test {
    use combine::{StreamOnce, Positioned};
    use super::TokenKind::{self, *};
    use super::TokenStream;

    fn tok_str(s: &str) -> Vec<&str> {
        let mut r = Vec::new();
        let mut s = TokenStream::new(s);
        loop {
            match s.uncons() {
                Ok(x) if x.kind == EndOfInput => break,
                Ok(x) => r.push(x.value),
                Err(e) => panic!("Parse error at {}: {}", s.position(), e),
            }
        }
        return r;
    }
    fn tok_typ(s: &str) -> Vec<TokenKind> {
        let mut r = Vec::new();
        let mut s = TokenStream::new(s);
        loop {
            match s.uncons() {
                Ok(x) if x.kind == EndOfInput => break,
                Ok(x) => r.push(x.kind),
                Err(e) => panic!("Parse error at {}: {}", s.position(), e),
            }
        }
        return r;
    }

    #[test]
    fn comments() {
        assert_eq!(tok_str("//hello"), &[] as &[&str]);
        assert_eq!(tok_str("//hello\n"), &[] as &[&str]);
        assert_eq!(tok_str("    "), &[] as &[&str]);
        assert_eq!(tok_str("  // hello  \n   "), &[] as &[&str]);
    }

    #[test]
    fn string() {
        assert_eq!(tok_str(r#""""#), [r#""""#]);
        assert_eq!(tok_typ(r#""""#), [String]);
        assert_eq!(tok_str(r#""hello""#), [r#""hello""#]);
        assert_eq!(tok_str(r#""hello\\""#), [r#""hello\\""#]);
        assert_eq!(tok_str(r#""hello\\\\""#), [r#""hello\\\\""#]);
        assert_eq!(tok_str(r#""he\\llo""#), [r#""he\\llo""#]);
        assert_eq!(tok_typ(r#""hello""#), [String]);
        assert_eq!(tok_str(r#""my\"quote""#), [r#""my\"quote""#]);
        assert_eq!(tok_typ(r#""my\"quote""#), [String]);
        assert_eq!(tok_str("\"hello\nworld\""), ["\"hello\nworld\""]);
        assert_eq!(tok_typ("\"hello\nworld\""), [String]);
    }
}
