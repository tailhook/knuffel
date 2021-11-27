use std::borrow::Cow;
use std::fmt;

use combine::easy::Errors;
use combine::stream::easy::Error as Err;

pub(crate) trait ResultExt<T, S: Clone> {
    fn err_span(self, s: &S) -> Result<T, Error<S>>;
}

#[derive(Debug)]
pub struct RawError<S> {
    span: S,
    unexpected: Option<String>,
    expected: Option<String>,
    messages: Vec<String>,
}

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

impl<S: fmt::Display + fmt::Debug> std::error::Error for RawError<S> {}

impl<S: fmt::Display> fmt::Display for RawError<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.span.fmt(f)?;
        ": ".fmt(f)?;
        if let Some(unexp) = &self.unexpected {
            unexp.fmt(f)?;
        }
        if let Some(exp) = &self.expected {
            if self.unexpected.is_some() {
                "; ".fmt(f)?;
            }
            exp.fmt(f)?;
            ".".fmt(f)?;
        } else {
            ".".fmt(f)?;
        }
        for msg in &self.messages {
            "\n  ".fmt(f)?;
            msg.fmt(f)?;
        }
        Ok(())
    }
}

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
    pub fn from_err<E>(span: &S, err: E) -> Error<S>
        where E: std::error::Error + Send + Sync + 'static,
    {
        Error {
            span: Some(span.clone()),
            inner: InnerError::Wraps(Box::new(err)),
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

impl<S, T, R> From<Errors<T, R, S>> for RawError<S>
    where T: fmt::Display, R: fmt::Display, S: fmt::Debug
{
    fn from(error: Errors<T, R, S>) -> RawError<S> {
        use std::fmt::Write;

        let unexpected = error.errors.iter().filter_map(|e| {
            if let Err::Unexpected(info) = e {
                Some(format!("unexpected `{}`",
                             info.to_string().escape_default()))
            } else {
                None
            }
        }).collect::<Vec<_>>();
        debug_assert!(unexpected.len() <= 1);

        let all_expected = error.errors.iter().filter_map(|e| {
            if let Err::Expected(info) = e {
                Some(info.to_string())
            } else {
                None
            }
        }).collect::<Vec<_>>();
        let expected = if all_expected.len() > 0 {
            let mut expected = String::from("expected ");
            write!(&mut expected, "`{}`",
                   all_expected[0].to_string().escape_default()).unwrap();
            if all_expected.len() > 1 {
                for item in &all_expected[1..all_expected.len()-1] {
                    write!(&mut expected, ", `{}`",
                           item.to_string().escape_default()).unwrap();
                }
                write!(&mut expected, "or `{}`",
                       all_expected.last().unwrap()
                       .to_string().escape_default()).unwrap();
            } else {
            }
            Some(expected)
        } else {
            None
        };

        let messages = error.errors.iter().filter_map(|e| {
            if let Err::Message(msg) = e {
                Some(msg.to_string())
            } else {
                None
            }
        }).collect::<Vec<_>>();

        RawError {
            span: error.position,
            unexpected: {unexpected}.pop(),
            expected,
            messages,
        }
    }
}
