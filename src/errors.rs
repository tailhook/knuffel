//! Error types for the knuffel library
//!
//! You only need [`Error`](enum@Error) exposed as `knuffel::Error` unless you
//! do manual implementations of any of the `Decode*` traits.
use std::borrow::Cow;
use std::collections::BTreeSet;
use std::fmt::{self, Write};

use thiserror::Error;
use miette::{Diagnostic, NamedSource};

use crate::ast::{TypeName, Literal, SpannedNode};
use crate::span::{Spanned};
use crate::decode::Kind;
use crate::traits::{ErrorSpan, Span};


/// Main error that is returned from KDL parsers
///
/// Implements [`miette::Diagnostic`] so can be used to print nice error
/// output with code snippets.
///
/// See [crate documentation](crate#Errors) and [miette} documentation to
/// find out how deal with them.
#[derive(Debug, Diagnostic, Error)]
#[error("error parsing KDL")]
pub struct Error {
    #[source_code]
    pub(crate) source_code: NamedSource,
    #[related]
    pub(crate) errors: Vec<miette::Report>,
}

/// An error type that is returned by decoder traits and emitted to the context
///
/// These are elements of the
#[derive(Debug, Diagnostic, Error)]
#[non_exhaustive]
pub enum DecodeError<S: ErrorSpan> {
    /// Unexpected type name encountered
    ///
    /// Type names are identifiers and strings in parenthesis before node names
    /// or values.
    #[error("{} for {}, found {}", expected, rust_type,
            found.as_ref().map(|x| x.as_str()).unwrap_or("no type name"))]
    #[diagnostic()]
    TypeName {
        /// Position of the type name
        #[label="unexpected type name"]
        span: S,
        /// Type name contained in the source code
        found: Option<TypeName>,
        /// Expected type name or type names
        expected: ExpectedType,
        /// Rust type that is being decoded when error is encountered
        rust_type: &'static str,
    },
    /// Different scalar kind was encountered than expected
    ///
    /// This is emitted when integer is used instead of string, and similar. It
    /// may also be encountered when `null` is used for non-optional field.
    #[diagnostic()]
    #[error("expected {} scalar, found {}", expected, found)]
    ScalarKind {
        /// Position of the unexpected scalar
        #[label("unexpected {}", found)]
        span: S,
        /// Scalar kind (or multiple) expected at this position
        expected: ExpectedKind,
        /// Kind of scalar that is found
        found: Kind,
    },
    /// Some required element is missing
    ///
    /// This is emitted on missing required attributes, properties, or children.
    /// (missing type names are emitted using [`DecodeError::TypeName`])
    #[diagnostic()]
    #[error("{}", message)]
    Missing {
        /// Position of the node name of which has missing element
        #[label("node starts here")]
        span: S,
        /// Description of what's missing
        message: String,
    },
    /// Missing named node at top level
    ///
    /// This is similar to `Missing` but is only emitted for nodes on the
    /// document level. This is separate error because there is no way to show
    /// span where missing node is expected (end of input is not very helpful).
    #[diagnostic()]
    #[error("{}", message)]
    MissingNode {
        /// Descriptino of what's missing
        message: String,
    },
    /// Unexpected entity encountered
    ///
    /// This is emitted for entities (arguments, properties, children) that have
    /// to matching structure field to put into, and also for nodes that aren
    /// expected to be encountered twice.
    #[diagnostic()]
    #[error("{}", message)]
    Unexpected {
        /// Position of the unexpected element
        #[label("unexpected {}", kind)]
        span: S,
        /// Kind of element that was found
        kind: &'static str,
        /// Description of the error
        message: String,
    },
    /// Bad scalar conversion
    ///
    /// This error is emitted when some scalar value of right kind cannot be
    /// converted to the Rust value. Including, but not limited to:
    /// 1. Integer value out of range
    /// 2. `FromStr` returned error for the value parse by
    ///    `#[knuffel(.., str)]`
    #[error("{}", error)]
    #[diagnostic()]
    Conversion {
        /// Position of the scalar that could not be converted
        #[label("invalid value")]
        span: S,
        /// Original error
        error: Box<dyn std::error::Error + Send + Sync + 'static>,
    },
    /// Unsupported value
    ///
    /// This is currently used to error out on `(base64)` values when `base64`
    /// feature is not enabled.
    #[error("{}", message)]
    #[diagnostic()]
    Unsupported {
        /// Position of the value that is unsupported
        #[label="unsupported value"]
        span: S,
        /// Description of why the value is not supported
        message: Cow<'static, str>,
    },
    /// Custom error that can be emitted during decoding
    ///
    /// This is not used by the knuffel itself. Note most of the time it's
    /// better to use [`DecodeError::Conversion`] as that will associate
    /// source code span to the error.
    #[error(transparent)]
    Custom(Box<dyn std::error::Error + Send + Sync + 'static>),
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
pub(crate) enum ParseError<S: ErrorSpan> {
    #[error("{}", FormatUnexpected(found, expected))]
    #[diagnostic()]
    Unexpected {
        label: Option<&'static str>,
        #[label("{}", label.unwrap_or("unexpected token"))]
        span: S,
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
        span: S,
        message: String,
    },
    #[error("{}", message)]
    #[diagnostic(help("{}", help))]
    MessageWithHelp {
        label: Option<&'static str>,
        #[label("{}", label.unwrap_or("unexpected token"))]
        span: S,
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

impl<S: ErrorSpan> ParseError<S> {
    pub(crate) fn with_expected_token(mut self, token: &'static str) -> Self {
        use ParseError::*;
        match &mut self {
            Unexpected { ref mut expected, .. } => {
                *expected = [TokenFormat::Token(token)].into_iter().collect();
            }
            _ => {},
        }
        self
    }
    pub(crate) fn with_expected_kind(mut self, token: &'static str) -> Self {
        use ParseError::*;
        match &mut self {
            Unexpected { ref mut expected, .. } => {
                *expected = [TokenFormat::Kind(token)].into_iter().collect();
            }
            _ => {},
        }
        self
    }
    pub(crate) fn with_no_expected(mut self) -> Self {
        use ParseError::*;
        match &mut self {
            Unexpected { ref mut expected, .. } => {
                *expected = BTreeSet::new();
            }
            _ => {},
        }
        self
    }
    #[allow(dead_code)]
    pub(crate) fn map_span<T>(self, f: impl Fn(S) -> T) -> ParseError<T>
        where T: ErrorSpan,
    {
        use ParseError::*;
        match self {
            Unexpected { label, span, found, expected }
            => Unexpected { label, span: f(span), found, expected },
            Unclosed { label, opened_at, opened, expected_at, expected, found }
            => Unclosed { label, opened_at: f(opened_at), opened,
                          expected_at: f(expected_at), expected, found },
            Message { label, span, message }
            => Message { label, span: f(span), message },
            MessageWithHelp { label, span, message, help }
            => MessageWithHelp { label, span: f(span), message, help },
        }
    }
}

impl<S: Span> chumsky::Error<char> for ParseError<S> {
    type Span = S;
    type Label = &'static str;
    fn expected_input_found<Iter>(span: Self::Span, expected: Iter,
        found: Option<char>)
        -> Self
        where Iter: IntoIterator<Item = Option<char>>
    {
        ParseError::Unexpected {
            label: None,
            span,
            found: found.into(),
            expected: expected.into_iter().map(Into::into).collect(),
        }
    }
    fn with_label(mut self, new_label: Self::Label) -> Self {
        use ParseError::*;
        match self {
            Unexpected { ref mut label, .. } => *label = Some(new_label),
            Unclosed { ref mut label, .. } => *label = new_label,
            Message { ref mut label, .. } => *label = Some(new_label),
            MessageWithHelp { ref mut label, .. } => *label = Some(new_label),
        }
        self
    }
    fn merge(mut self, other: Self) -> Self {
        use ParseError::*;
        match (&mut self, other) {
            (Unclosed { .. }, _) => self,
            (_, other@Unclosed { .. }) => other,
            (Unexpected { expected: ref mut dest, .. },
             Unexpected { expected, .. })
            => {
                dest.extend(expected.into_iter());
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
        ParseError::Unclosed {
            label: "delimited",
            opened_at: unclosed_span,
            opened: unclosed.into(),
            expected_at: span,
            expected: expected.into(),
            found: found.into(),
        }
    }
}

impl<S: ErrorSpan> DecodeError<S> {
    /// Construct [`DecodeError::Conversion`] error
    pub fn conversion<T, E>(span: &Spanned<T, S>, err: E) -> Self
        where E: Into<Box<dyn std::error::Error + Send + Sync + 'static>>,
    {
        DecodeError::Conversion {
            span: span.span().clone(),
            error: err.into(),
        }
    }
    /// Construct [`DecodeError::ScalarKind`] error
    pub fn scalar_kind(expected: Kind, found: &Spanned<Literal, S>) -> Self {
        DecodeError::ScalarKind {
            span: found.span().clone(),
            expected: expected.into(),
            found: (&found.value).into(),
        }
    }
    /// Construct [`DecodeError::Missing`] error
    pub fn missing(node: &SpannedNode<S>, message: impl Into<String>) -> Self {
        DecodeError::Missing {
            span: node.node_name.span().clone(),
            message: message.into(),
        }
    }
    /// Construct [`DecodeError::Unexpected`] error
    pub fn unexpected<T>(elem: &Spanned<T, S>, kind: &'static str,
                         message: impl Into<String>)
        -> Self
    {
        DecodeError::Unexpected {
            span: elem.span().clone(),
            kind,
            message: message.into(),
        }
    }
    /// Construct [`DecodeError::Unsupported`] error
    pub fn unsupported<T, M>(span: &Spanned<T, S>, message: M)-> Self
        where M: Into<Cow<'static, str>>,
    {
        DecodeError::Unsupported {
            span: span.span().clone(),
            message: message.into(),
        }
    }
    #[allow(dead_code)]
    pub(crate) fn map_span<T>(self, mut f: impl FnMut(S) -> T)
        -> DecodeError<T>
        where T: ErrorSpan,
    {
        use DecodeError::*;
        match self {
            TypeName { span, found, expected, rust_type }
            => TypeName { span: f(span), found, expected, rust_type },
            ScalarKind { span, expected, found }
            => ScalarKind { span: f(span), expected, found },
            Missing { span, message }
            => Missing { span: f(span), message},
            MissingNode { message }
            => MissingNode { message },
            Unexpected { span, kind, message }
            => Unexpected { span: f(span), kind, message},
            Conversion { span, error }
            => Conversion { span: f(span), error },
            Unsupported { span, message }
            => Unsupported { span: f(span), message },
            Custom(e) => Custom(e),
        }
    }
}

/// Wrapper around expected type that is used in [`DecodeError::TypeName`].
#[derive(Debug)]
pub struct ExpectedType {
    types: Vec<TypeName>,
    no_type: bool,
}

impl ExpectedType {
    /// Declare that decoder expects no type (no parens at all) for the value
    pub fn no_type() -> Self {
        ExpectedType {
            types: [].into(),
            no_type: true,
        }
    }
    /// Declare the type that has to be attached to the value
    pub fn required(ty: impl Into<TypeName>) -> Self {
        ExpectedType {
            types: vec![ty.into()],
            no_type: false,
        }
    }
    /// Declare the type that can be attached to the value
    ///
    /// But no type is also okay in this case (although, "no type" and specified
    /// type can potentially have different meaning).
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
            let last = if self.no_type {
                None
            } else {
                iter.next_back()
            };
            for item in iter {
                write!(f, ", {}", item)?;
            }
            if self.no_type {
                write!(f, " or no type")?;
            } else if let Some(last) = last {
                write!(f, " or {}", last)?;
            }
            Ok(())
        }
    }
}


/// Declares kind of value expected for the scalar value
///
/// Use [`Kind`](crate::decode::Kind) and `.into()` to create this value.
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
