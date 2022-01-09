use std::borrow::Cow;
use std::collections::BTreeSet;
use std::fmt::{self, Write};

use thiserror::Error;
use miette::Diagnostic;

use crate::ast::{TypeName, Literal};
use crate::span::Spanned;
use crate::decode::Kind;
use crate::traits::Span;


#[derive(Debug, Diagnostic, Error)]
#[error("{}", error)]
#[diagnostic(forward(error))]
pub(crate) struct AddSource<E: Diagnostic + 'static> {
    #[source_code]
    pub source_code: std::sync::Arc<String>,
    pub error: E,
}

#[derive(Debug, Diagnostic, Error)]
#[non_exhaustive]
pub enum Error {
    #[error("syntax error")]
    #[diagnostic(transparent)]
    Syntax(Box<dyn Diagnostic + Send + Sync + 'static>),
    #[error("decode error")]
    #[diagnostic(transparent)]
    Decode(Box<dyn Diagnostic + Send + Sync + 'static>),
}

#[derive(Debug, Diagnostic, Error)]
pub enum DecodeError<S: Span> {
    #[error("{} for {}, found {}", expected, rust_type,
            found.as_ref().map(|x| x.as_str()).unwrap_or("no type name"))]
    #[diagnostic()]
    TypeName {
        #[label="unexpected type name"]
        span: S,
        found: Option<TypeName>,
        expected: ExpectedType,
        rust_type: &'static str,
    },
    #[diagnostic()]
    #[error("expected {} scalar, found {}", expected, found)]
    ScalarKind {
        #[label("unexpected {}", found)]
        span: S,
        expected: ExpectedKind,
        found: Kind,
    },
    #[error("{}", source)]
    #[diagnostic()]
    Conversion {
        #[label("invalid value")]
        span: S,
        source: Box<dyn std::error::Error + Send + Sync + 'static>,
    },
    #[error("{}", message)]
    #[diagnostic()]
    Unsupported {
        #[label="unsupported value"]
        span: S,
        message: Cow<'static, str>,
    },
    #[error(transparent)]
    Custom(Box<dyn std::error::Error + Send + Sync + 'static>),
}


#[derive(Debug, Diagnostic, Error)]
#[error("KDL syntax error")]
#[diagnostic()]
pub(crate) struct ParseError<S: Span> {
    #[related]
    pub(crate) errors: Vec<AddSource<ParseErrorEnum<S>>>,
}

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub(crate) enum TokenFormat {
    Char(char),
    Token(&'static str),
    Kind(&'static str),
    OpenRaw(usize),
    CloseRaw(usize),
    Eoi,
}

struct FormatUnexpected<'x>(&'x TokenFormat, &'x BTreeSet<TokenFormat>);

#[derive(Debug, Diagnostic, Error)]
pub(crate) enum ParseErrorEnum<S: Span> {
    #[error("{}", FormatUnexpected(found, expected))]
    #[diagnostic()]
    Unexpected {
        label: Option<&'static str>,
        #[label("{}", label.unwrap_or("unexpected token"))]
        position: S,
        found: TokenFormat,
        expected: BTreeSet<TokenFormat>,
    },
    #[error("unclosed {} {}", label, opened)]
    #[diagnostic()]
    Unclosed {
        label: &'static str,
        #[label="opened here"]
        opened_at: S,
        opened: TokenFormat,
        #[label("expected {}", expected)]
        expected_at: S,
        expected: TokenFormat,
        found: TokenFormat,
    },
    #[error("{}", message)]
    #[diagnostic()]
    Message {
        label: Option<&'static str>,
        #[label("{}", label.unwrap_or("unexpected token"))]
        position: S,
        message: String,
    },
    #[error("{}", message)]
    #[diagnostic(help("{}", help))]
    MessageWithHelp {
        label: Option<&'static str>,
        #[label("{}", label.unwrap_or("unexpected token"))]
        position: S,
        message: String,
        help: &'static str,
    },
}


impl From<Option<char>> for TokenFormat {
    fn from(chr: Option<char>) -> TokenFormat {
        if let Some(chr) = chr {
            TokenFormat::Char(chr)
        } else {
            TokenFormat::Eoi
        }
    }
}

impl From<char> for TokenFormat {
    fn from(chr: char) -> TokenFormat {
        TokenFormat::Char(chr)
    }
}

impl From<&'static str> for TokenFormat {
    fn from(s: &'static str) -> TokenFormat {
        TokenFormat::Token(s)
    }
}

impl fmt::Display for TokenFormat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TokenFormat::*;
        match self {
            // do not escape quotes as we use backticks
            Char('"') => write!(f, "`\"`"),
            Char('\'') => write!(f, "`\'`"),
            // also single backslash should not confuse anybody in this context
            Char('\\') => write!(f, r"`\`"),

            Char(c) => write!(f, "`{}`", c.escape_default()),
            Token(s) => write!(f, "`{}`", s.escape_default()),
            Kind(s) => write!(f, "{}", s),
            Eoi => write!(f, "end of input"),
            OpenRaw(0) => {
                f.write_str("`r\"`")
            }
            OpenRaw(n) => {
                f.write_str("`r")?;
                for _ in 0..*n {
                    f.write_char('#')?;
                }
                f.write_str("\"`")
            }
            CloseRaw(0) => {
                f.write_str("`\"`")
            }
            CloseRaw(n) => {
                f.write_str("`\"")?;
                for _ in 0..*n {
                    f.write_char('#')?;
                }
                f.write_char('`')
            }
        }
    }
}

impl fmt::Display for FormatUnexpected<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "found {}", self.0)?;
            let mut iter = self.1.iter();
        if let Some(item) = iter.next() {
            write!(f, ", expected {}", item)?;
            let back = iter.next_back();
            for item in iter {
                write!(f, ", {}", item)?;
            }
            if let Some(item) = back {
                write!(f, " or {}", item)?;
            }
        }
        Ok(())
    }
}

impl<S: Span> ParseErrorEnum<S> {
    pub(crate) fn span(&self) -> &S {
        use ParseErrorEnum::*;
        match self {
            Unexpected { position, .. } => position,
            Unclosed { expected_at, .. } => expected_at,
            Message { position, .. } => position,
            MessageWithHelp { position, .. } => position,
        }
    }
    pub(crate) fn with_expected_token(mut self, token: &'static str) -> Self {
        use ParseErrorEnum::*;
        match &mut self {
            Unexpected { ref mut expected, .. } => {
                *expected = [TokenFormat::Token(token)].into_iter().collect();
            }
            _ => {},
        }
        self
    }
    pub(crate) fn with_expected_kind(mut self, token: &'static str) -> Self {
        use ParseErrorEnum::*;
        match &mut self {
            Unexpected { ref mut expected, .. } => {
                *expected = [TokenFormat::Kind(token)].into_iter().collect();
            }
            _ => {},
        }
        self
    }
    pub(crate) fn with_no_expected(mut self) -> Self {
        use ParseErrorEnum::*;
        match &mut self {
            Unexpected { ref mut expected, .. } => {
                *expected = BTreeSet::new();
            }
            _ => {},
        }
        self
    }
}

impl<S: Span> chumsky::Error<char> for ParseErrorEnum<S> {
    type Span = S;
    type Label = &'static str;
    fn expected_input_found<Iter>(span: Self::Span, expected: Iter,
        found: Option<char>)
        -> Self
        where Iter: IntoIterator<Item = Option<char>>
    {
        ParseErrorEnum::Unexpected {
            label: None,
            position: span,
            found: found.into(),
            expected: expected.into_iter().map(Into::into).collect(),
        }
    }
    fn with_label(mut self, new_label: Self::Label) -> Self {
        use ParseErrorEnum::*;
        match self {
            Unexpected { ref mut label, .. } => *label = Some(new_label),
            Unclosed { ref mut label, .. } => *label = new_label,
            Message { ref mut label, .. } => *label = Some(new_label),
            MessageWithHelp { ref mut label, .. } => *label = Some(new_label),
        }
        self
    }
    fn merge(mut self, other: Self) -> Self {
        use ParseErrorEnum::*;
        match (&mut self, other) {
            (Unclosed { .. }, _) => self,
            (_, other@Unclosed { .. }) => other,
            (Unexpected { expected: ref mut dest, .. },
             Unexpected { position, expected, .. })
            => {
                dest.extend(expected.into_iter());
                assert_eq!(self.span().start(), position.start());
                self
            }
            (_, other) => todo!("{} -> {}", self, other),
        }
    }
    fn unclosed_delimiter(
        unclosed_span: Self::Span,
        unclosed: char,
        span: Self::Span,
        expected: char,
        found: Option<char>
    ) -> Self {
        ParseErrorEnum::Unclosed {
            label: "delimited",
            opened_at: unclosed_span,
            opened: unclosed.into(),
            expected_at: span,
            expected: expected.into(),
            found: found.into(),
        }
    }
}

impl<S: Span> DecodeError<S> {
    pub fn conversion<T, E>(span: &Spanned<T, S>, err: E) -> Self
        where E: Into<Box<dyn std::error::Error + Send + Sync + 'static>>,
    {
        DecodeError::Conversion {
            span: span.span().clone(),
            source: err.into(),
        }
    }
    pub fn scalar_kind(expected: Kind, found: &Spanned<Literal, S>) -> Self {
        DecodeError::ScalarKind {
            span: found.span().clone(),
            expected: expected.into(),
            found: (&found.value).into(),
        }
    }
    pub fn unsupported<T, M>(span: &Spanned<T, S>, message: M)-> Self
        where M: Into<Cow<'static, str>>,
    {
        DecodeError::Unsupported {
            span: span.span().clone(),
            message: message.into(),
        }
    }
}

#[derive(Debug)]
pub struct ExpectedType {
    types: Vec<TypeName>,
    no_type: bool,
}

impl ExpectedType {
    pub fn no_type() -> Self {
        ExpectedType {
            types: [].into(),
            no_type: true,
        }
    }
    pub fn required(ty: impl Into<TypeName>) -> Self {
        ExpectedType {
            types: vec![ty.into()],
            no_type: false,
        }
    }
    pub fn optional(ty: impl Into<TypeName>) -> Self {
        ExpectedType {
            types: vec![ty.into()],
            no_type: true,
        }
    }
}

impl fmt::Display for ExpectedType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.types.is_empty() {
            write!(f, "no type")
        } else {
            let mut iter = self.types.iter();
            if let Some(first) = iter.next() {
                write!(f, "{}", first)?;
            }
            let last = iter.next_back();
            for item in iter {
                write!(f, ", {}", item)?;
            }
            if let Some(last) = last {
                write!(f, " or {}", last)?;
            }
            Ok(())
        }
    }
}


#[derive(Debug)]
pub struct ExpectedKind(Kind);

impl From<Kind> for ExpectedKind {
    fn from(kind: Kind) -> ExpectedKind {
        ExpectedKind(kind)
    }
}

impl fmt::Display for ExpectedKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0.as_str())
    }
}
