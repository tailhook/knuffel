use std::str::FromStr;
use std::path::PathBuf;

use crate::ast::{Literal, Integer, Radix, TypeName, BuiltinType};
use crate::span::{Spanned};
use crate::errors::{Error, ResultExt};
use crate::traits::{Span, DecodeScalar};

macro_rules! impl_integer {
    ($typ: ident, $marker: ident) => {
        impl TryFrom<&Integer> for $typ {
            type Error = <$typ as FromStr>::Err;
            fn try_from(val: &Integer) -> Result<$typ, <$typ as FromStr>::Err>
            {
                match val.0 {
                    Radix::Bin => <$typ>::from_str_radix(&val.1, 2),
                    Radix::Oct => <$typ>::from_str_radix(&val.1, 8),
                    Radix::Dec => <$typ>::from_str(&val.1),
                    Radix::Hex => <$typ>::from_str_radix(&val.1, 16),
                }
            }
        }

        impl<S: Span> DecodeScalar<S> for $typ {
            fn raw_decode(val: &Spanned<Literal, S>) -> Result<$typ, Error<S>>
            {
                match &**val {
                    Literal::Int(ref value) => {
                        value.try_into().err_span(val.span())
                    }
                    _ => Err(Error::new(val.span(), "expected integer value")),
                }
            }
            fn type_check(type_name: &Option<Spanned<TypeName, S>>)
                -> Result<(), Error<S>>
            {
                if let Some(typ) = type_name {
                    if typ.as_builtin() != Some(&BuiltinType::$marker) {
                        return Err(Error::new(typ.span(),
                            format!(concat!("expected type `",
                                            stringify!($typ),
                                            "`, found `{}`"),
                                    typ.as_str().escape_default())
                        ));
                    }
                }
                Ok(())
            }
        }
    }
}

impl_integer!(i8, I8);
impl_integer!(u8, U8);
impl_integer!(i16, I16);
impl_integer!(u16, U16);
impl_integer!(i32, I32);
impl_integer!(u32, U32);
impl_integer!(i64, I64);
impl_integer!(u64, U64);

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
