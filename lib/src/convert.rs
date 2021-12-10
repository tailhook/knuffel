use std::str::FromStr;
use std::path::PathBuf;

use crate::ast::{Value, Literal, Integer, Radix};
use crate::span::{Spanned};
use crate::errors::{Error, ResultExt};
use crate::traits::{Span, DecodeScalar, DecodeTypedScalar};


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

impl<S: Span> DecodeScalar<S> for u64 {
    fn decode(val: &Spanned<Literal, S>) -> Result<u64, Error<S>> {
        match &**val {
            Literal::Int(ref value) => value.try_into().err_span(val.span()),
            _ => Err(Error::new(val.span(), "expected integer value")),
        }
    }
}

impl<S: Span> DecodeTypedScalar<S> for u64 {
    fn decode(val: &Value<S>) -> Result<u64, Error<S>> {
        if let Some(typ) = &val.type_name {
            if typ.as_str() != "u64" {
                return Err(Error::new(typ.span(),
                    format!("expected type `u64`, found `{}`", typ.as_str())));
            }
        }
        DecodeScalar::decode(&val.literal)
    }
}


impl<S: Span> DecodeScalar<S> for String {
    fn decode(val: &Spanned<Literal, S>) -> Result<String, Error<S>> {
        match &**val {
            Literal::String(ref s) => Ok(s.clone().into()),
            _ => Err(Error::new(val.span(), "expected string value")),
        }
    }
}

impl<S: Span> DecodeTypedScalar<S> for String {
    fn decode(val: &Value<S>) -> Result<String, Error<S>> {
        if let Some(typ) = &val.type_name {
            return Err(Error::new(typ.span(),
                format!("unexpected type name for String")));
        }
        DecodeScalar::decode(&val.literal)
    }
}

impl<S: Span> DecodeScalar<S> for PathBuf {
    fn decode(val: &Spanned<Literal, S>) -> Result<PathBuf, Error<S>> {
        match &**val {
            Literal::String(ref s) => Ok(String::from(s.clone()).into()),
            _ => Err(Error::new(val.span(), "expected string value")),
        }
    }
}

impl<S: Span> DecodeTypedScalar<S> for PathBuf {
    fn decode(val: &Value<S>) -> Result<PathBuf, Error<S>> {
        if let Some(typ) = &val.type_name {
            return Err(Error::new(typ.span(),
                format!("unexpected type name for PathBuf")));
        }
        DecodeScalar::decode(&val.literal)
    }
}

impl<S: Span> DecodeScalar<S> for bool {
    fn decode(val: &Spanned<Literal, S>) -> Result<bool, Error<S>> {
        match &**val {
            Literal::Bool(value) => Ok(*value),
            _ => Err(Error::new(val.span(), "expected integer value")),
        }
    }
}

impl<S: Span> DecodeTypedScalar<S> for bool {
    fn decode(val: &Value<S>) -> Result<bool, Error<S>> {
        if let Some(typ) = &val.type_name {
            return Err(Error::new(typ.span(),
                format!("unexpected type name for bool")));
        }
        DecodeScalar::decode(&val.literal)
    }
}
