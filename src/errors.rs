use std::fmt;
use combine::easy::Errors;
use combine::stream::easy::Error as Err;

#[derive(Debug)]
pub struct RawError<S> {
    span: S,
    unexpected: Option<String>,
    expected: Option<String>,
    messages: Vec<String>,
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

impl<S, T, R> From<Errors<T, R, S>> for RawError<S>
    where T: fmt::Display, R: fmt::Display,
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
