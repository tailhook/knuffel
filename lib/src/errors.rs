use std::borrow::Cow;
use std::collections::BTreeSet;
use std::fmt::{self, Write};

use thiserror::Error;
use miette::Diagnostic;

use crate::traits::Span;

pub(crate) trait ResultExt<T, S: Clone> {
    fn err_span(self, s: &S) -> Result<T, Error<S>>;
}

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
pub enum RealError {
    #[error("syntax error")]
    #[diagnostic(transparent)]
    Syntax(Box<dyn Diagnostic + Send + Sync + 'static>),
    #[error("decode error")]
    #[diagnostic(transparent)]
    Decode(Box<dyn Diagnostic + Send + Sync + 'static>),
}

#[derive(Debug, Diagnostic, Error)]
pub enum DecodeError<S: Span> {
    #[error("invalid type")]
    #[diagnostic()]
    TypeName { span: S },
    #[error("invalid scalar type")]
    #[diagnostic()]
    ScalarType {},
    #[error("invalid value")]
    #[diagnostic()]
    Convert {},
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

/*
#[derive(Debug, Diagnostic, Error)]
#[error("error converting value to type {}", type_name)]
struct Convert<E: std::error::Error + Send + Sync + 'static> {
    span: miette::SourceSpan,
    type_name: &'static str,
    #[source]
    error: E,
}
*/

#[derive(Debug)]
pub enum InnerError {
    Static(&'static str),
    Text(Box<str>),
    Wraps(Box<dyn std::error::Error + Send + Sync + 'static>),
}

#[derive(Debug)]
pub struct Error<S> {
    span: Option<S>,
    inner: InnerError,
}

impl<S: fmt::Display + fmt::Debug> std::error::Error for Error<S> {}

impl<S: Clone> Error<S>  {
    pub fn new(span: &S, text: impl Into<Cow<'static, str>>)
        -> Error<S>
    {
        match text.into() {
            Cow::Borrowed(txt) => {
                Error {
                    span: Some(span.clone()),
                    inner: InnerError::Static(txt),
                }
            }
            Cow::Owned(txt) => {
                Error {
                    span: Some(span.clone()),
                    inner: InnerError::Text(txt.into()),
                }
            }
        }
    }
    pub fn ensure_span(mut self, span: &S) -> Error<S> {
        if self.span.is_none() {
            self.span = Some(span.clone());
        }
        self
    }
    pub fn from_err<E>(span: &S, err: E) -> Error<S>
        where E: Into<Box<dyn std::error::Error + Send + Sync + 'static>>,
    {
        Error {
            span: Some(span.clone()),
            inner: InnerError::Wraps(err.into()),
        }
    }
    pub fn new_global(text: impl Into<Cow<'static, str>>) -> Error<S> {
        match text.into() {
            Cow::Borrowed(txt) => {
                Error {
                    span: None,
                    inner: InnerError::Static(txt),
                }
            }
            Cow::Owned(txt) => {
                Error {
                    span: None,
                    inner: InnerError::Text(txt.into()),
                }
            }
        }
    }
}

impl<S: fmt::Display> fmt::Display for Error<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(span) = &self.span {
            span.fmt(f)?;
            ": ".fmt(f)?;
        }
        match &self.inner {
            InnerError::Static(s) => s.fmt(f)?,
            InnerError::Text(s) => s.fmt(f)?,
            InnerError::Wraps(e) => e.fmt(f)?,
        }
        Ok(())
    }
}

impl<R, E, S: Clone> ResultExt<R, S> for Result<R, E>
    where E: std::error::Error + Send + Sync + 'static,
{
    fn err_span(self, s: &S) -> Result<R, Error<S>> {
        self.map_err(|e| Error::from_err(s, e))
    }
}
