use std::str::FromStr;
use crate::ast::{Value, Literal, Integer, Radix};
use crate::span::{Spanned};
use crate::errors::{Error, ResultExt};
use crate::traits::Span;

impl TryFrom<&Integer> for u64 {
    type Error = <u64 as FromStr>::Err;
    fn try_from(val: &Integer) -> Result<u64, <u64 as FromStr>::Err> {
        match val.0 {
            Radix::Bin => u64::from_str_radix(&val.1, 2),
            Radix::Oct => u64::from_str_radix(&val.1, 8),
            Radix::Dec => u64::from_str(&val.1),
            Radix::Hex => u64::from_str_radix(&val.1, 16),
        }
    }
}

impl<S: Span> TryFrom<&'_ Spanned<Literal, S>> for u64 {
    type Error = Error<S>;
    fn try_from(val: &Spanned<Literal, S>) -> Result<u64, Error<S>> {
        match &**val {
            Literal::Int(ref value) => value.try_into().err_span(val.span()),
            other => Err(Error::new(val.span(), "bad type")), // TODO
        }
    }
}

impl<S: Span> TryFrom<&'_ Value<S>> for u64 {
    type Error = Error<S>;
    fn try_from(val: &Value<S>) -> Result<u64, Error<S>> {
        if let Some(typ) = &val.type_name {
            if typ.as_str() != "u64" {
                return Err(Error::new(typ.span(),
                    format!("expected type `u64`, found `{}`", typ.as_str())));
            }
        }
        (&val.literal).try_into()
    }
}


impl<S: Span> TryFrom<&'_ Spanned<Literal, S>> for String {
    type Error = Error<S>;
    fn try_from(val: &Spanned<Literal, S>) -> Result<String, Error<S>> {
        match &**val {
            Literal::String(ref s) => Ok(s.clone().into()),
            other => Err(Error::new(val.span(), "bad type")), // TODO
        }
    }
}

impl<S: Span> TryFrom<&'_ Value<S>> for String {
    type Error = Error<S>;
    fn try_from(val: &Value<S>) -> Result<String, Error<S>> {
        if let Some(typ) = &val.type_name {
            if typ.as_str() != "str" {
                return Err(Error::new(typ.span(),
                    format!("expected type `str`, found `{}`", typ.as_str())));
            }
        }
        (&val.literal).try_into()
    }
}
