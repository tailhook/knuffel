use std::str::FromStr;
use std::path::PathBuf;

use crate::ast::{Literal, Integer, Radix, TypeName, BuiltinType};
use crate::span::{Spanned};
use crate::errors::{Error, ResultExt};
use crate::traits::{Span, DecodeScalar};


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

impl TryFrom<&Integer> for i64 {
    type Error = <i64 as FromStr>::Err;
    fn try_from(val: &Integer) -> Result<i64, <i64 as FromStr>::Err> {
        match val.0 {
            Radix::Bin => i64::from_str_radix(&val.1, 2),
            Radix::Oct => i64::from_str_radix(&val.1, 8),
            Radix::Dec => i64::from_str(&val.1),
            Radix::Hex => i64::from_str_radix(&val.1, 16),
        }
    }
}

impl<S: Span> DecodeScalar<S> for u64 {
    fn raw_decode(val: &Spanned<Literal, S>) -> Result<u64, Error<S>> {
        match &**val {
            Literal::Int(ref value) => value.try_into().err_span(val.span()),
            _ => Err(Error::new(val.span(), "expected integer value")),
        }
    }
    fn type_check(type_name: &Option<Spanned<TypeName, S>>)
        -> Result<(), Error<S>>
    {
        if let Some(typ) = type_name {
            if typ.as_builtin() != Some(&BuiltinType::U64) {
                return Err(Error::new(typ.span(),
                    format!("expected type `u64`, found `{}`", typ.as_str())));
            }
        }
        Ok(())
    }
}

impl<S: Span> DecodeScalar<S> for i64 {
    fn raw_decode(val: &Spanned<Literal, S>) -> Result<i64, Error<S>> {
        match &**val {
            Literal::Int(ref value) => value.try_into().err_span(val.span()),
            _ => Err(Error::new(val.span(), "expected integer value")),
        }
    }
    fn type_check(type_name: &Option<Spanned<TypeName, S>>)
        -> Result<(), Error<S>>
    {
        if let Some(typ) = type_name {
            if typ.as_builtin() != Some(&BuiltinType::I64) {
                return Err(Error::new(typ.span(),
                    format!("expected type `u64`, found `{}`", typ.as_str())));
            }
        }
        Ok(())
    }
}

impl<S: Span> DecodeScalar<S> for String {
    fn raw_decode(val: &Spanned<Literal, S>) -> Result<String, Error<S>> {
        match &**val {
            Literal::String(ref s) => Ok(s.clone().into()),
            _ => Err(Error::new(val.span(), "expected string value")),
        }
    }
    fn type_check(type_name: &Option<Spanned<TypeName, S>>)
        -> Result<(), Error<S>>
    {
        if let Some(typ) = type_name {
            return Err(Error::new(typ.span(),
                format!("unexpected type name for String")));
        }
        Ok(())
    }
}


impl<S: Span> DecodeScalar<S> for PathBuf {
    fn raw_decode(val: &Spanned<Literal, S>) -> Result<PathBuf, Error<S>> {
        match &**val {
            Literal::String(ref s) => Ok(String::from(s.clone()).into()),
            _ => Err(Error::new(val.span(), "expected string value")),
        }
    }
    fn type_check(type_name: &Option<Spanned<TypeName, S>>)
        -> Result<(), Error<S>>
    {
        if let Some(typ) = type_name {
            return Err(Error::new(typ.span(),
                format!("unexpected type name for PathBuf")));
        }
        Ok(())
    }
}

impl<S: Span> DecodeScalar<S> for bool {
    fn raw_decode(val: &Spanned<Literal, S>) -> Result<bool, Error<S>> {
        match &**val {
            Literal::Bool(value) => Ok(*value),
            _ => Err(Error::new(val.span(), "expected integer value")),
        }
    }
    fn type_check(type_name: &Option<Spanned<TypeName, S>>)
        -> Result<(), Error<S>>
    {
        if let Some(typ) = type_name {
            return Err(Error::new(typ.span(),
                format!("unexpected type name for bool")));
        }
        Ok(())
    }
}
