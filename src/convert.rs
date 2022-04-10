use std::str::FromStr;
use std::path::PathBuf;
use std::default::Default;

use crate::ast::{Literal, Integer, Decimal, Radix, TypeName, BuiltinType};
use crate::decode::{Context, Kind};
use crate::errors::{DecodeError, ExpectedType};
use crate::span::{Spanned};
use crate::traits::{ErrorSpan, DecodeScalar};


macro_rules! impl_number {
    // Matches a repeating pattern of
    // `(<type_name>, <number_type>, <marker>)` followed by a comma,
    // where `<type_name>` is one of `Integer` or `Decimal`,
    // and `<number_type>` is a Rust type (for example `i32` or `f64`),
    // and `<marker>` is the variant of `BuiltinType` that matches `<number_type>`,
    // (for example `I32` or `F64`).
    ($(($type_name: ident, $number_type: ident, $marker: ident),)+) => {
        // Repeat this recursion for every matched pattern, where the pattern
        // is in parentheses followed by a comma as described above.
        $(
            impl_number!($type_name, $number_type, $marker);
        )*
        // The asterisk will repeat the contents inside parentheses zero-or-more
        // times, as many times as the left-hand side of the pattern branch has matched
        // a pair of parentheses with arguments described.
    };
    // Matches one pattern of `(Integer, <number_type>, <marker>)`,
    // with `Integer` in the position of `<type_name>`.
    // This is called recursively by the first match pattern.
    // Handles the implementation of `TryFrom<&Integer>` when `<type_name>` is `Integer`
    // for the `<number_type>` (a Rust type),
    // where `<marker_type>` corresponds to `<number_type>`.
    (Integer, $number_type: ident, $marker: ident) => {
        impl TryFrom<&Integer> for $number_type {
            type Error = <$number_type as FromStr>::Err;
            fn try_from(val: &Integer) -> Result<$number_type, <$number_type as FromStr>::Err>
            {
                match val.0 {
                    Radix::Bin => <$number_type>::from_str_radix(&val.1, 2),
                    Radix::Oct => <$number_type>::from_str_radix(&val.1, 8),
                    Radix::Dec => <$number_type>::from_str(&val.1),
                    Radix::Hex => <$number_type>::from_str_radix(&val.1, 16),
                }
            }
        }

        // Because `<number_type>` is `Integer` it cannot be used as the `<lit_variant>`
        // and must be renamed to `Int` and passed recursively.
        impl_number!(_ImplDecode, Int, $number_type, $marker, 0);
    };
    // Matches one pattern of `(Decimal, <number_type>, <marker>)`
    // with `Decimal` in the position of `<type_name>`.
    // This is called recursively by the first match pattern.
    // Handles the implementation of `TryFrom<&Decimal>` when `<type_name>` is `Decimal`
    // for the `<number_type>` (a Rust type),
    // where `<marker_type>` corresponds to `<number_type>`.
    (Decimal, $number_type: ident, $marker: ident) => {
        impl TryFrom<&Decimal> for $number_type {
            type Error = <$number_type as FromStr>::Err;
            fn try_from(val: &Decimal) -> Result<$number_type, <$number_type as FromStr>::Err>
            {
                <$number_type>::from_str(&val.0)
            }
        }

        // Because `<number_type>` is `Decimal` it could be used as the `<lit_variant>`,
        // but to be consistent with the branch where `<number_type>` is `Integer`,
        // it is named directly as `Decimal`.
        impl_number!(_ImplDecode, Decimal, $number_type, $marker, 0.0);
    };
    // This is a "private" pattern that matches
    // one pattern of `(_ImplDecode, <lit_variant>, <number_type>, <marker>, <default>)`
    // where `<lit_variant>` is a variant of `Literal`, passed by a previous branch that
    // matched a `<type_name>`,
    // and `<number_type>` and `<marker>` are described above on the first pattern,
    // and the additional `<default>` is a hard-coded literal
    // that will be used when a value does not parse into the `<number_type>`.
    // Handles the implementation of `DecodeScalar` for the `<number_type>`.
    (_ImplDecode, $lit_variant: ident, $number_type: ident, $marker: ident, $default: expr) => {
        impl<S: ErrorSpan> DecodeScalar<S> for $number_type {
            fn raw_decode(val: &Spanned<Literal, S>, ctx: &mut Context<S>)
                -> Result<$number_type, DecodeError<S>>
            {
                match &**val {
                    Literal::$lit_variant(ref value) => {
                        match value.try_into() {
                            Ok(val) => Ok(val),
                            Err(e) => {
                                ctx.emit_error(DecodeError::conversion(val, e));
                                Ok($default)
                            }
                        }
                    }
                    _ => {
                        ctx.emit_error(DecodeError::scalar_kind(
                                Kind::String, val));
                        Ok($default)
                    }
                }
            }
            fn type_check(type_name: &Option<Spanned<TypeName, S>>,
                          ctx: &mut Context<S>)
            {
                if let Some(typ) = type_name {
                    if typ.as_builtin() != Some(&BuiltinType::$marker) {
                        ctx.emit_error(DecodeError::TypeName {
                            span: typ.span().clone(),
                            found: Some(typ.value.clone()),
                            expected: ExpectedType::optional(
                                BuiltinType::$marker),
                            rust_type: stringify!($typ),
                        });
                    }
                }
            }
        }
    };
}

impl_number!(
    (Integer, i8, I8),
    (Integer, u8, U8),
    (Integer, i16, I16),
    (Integer, u16, U16),
    (Integer, i32, I32),
    (Integer, u32, U32),
    (Integer, i64, I64),
    (Integer, u64, U64),
    (Integer, isize, Isize),
    (Integer, usize, Usize),
    (Decimal, f32, F32),
    (Decimal, f64, F64),
);

impl<S: ErrorSpan> DecodeScalar<S> for String {
    fn raw_decode(val: &Spanned<Literal, S>, ctx: &mut Context<S>)
        -> Result<String, DecodeError<S>>
    {
        match &**val {
            Literal::String(ref s) => Ok(s.clone().into()),
            _ => {
                ctx.emit_error(DecodeError::scalar_kind(Kind::String, val));
                Ok(String::new())
            }
        }
    }
    fn type_check(type_name: &Option<Spanned<TypeName, S>>,
                  ctx: &mut Context<S>)
    {
        if let Some(typ) = type_name {
            ctx.emit_error(DecodeError::TypeName {
                span: typ.span().clone(),
                found: Some(typ.value.clone()),
                expected: ExpectedType::no_type(),
                rust_type: "String",
            });
        }
    }
}


impl<S: ErrorSpan> DecodeScalar<S> for PathBuf {
    fn raw_decode(val: &Spanned<Literal, S>, ctx: &mut Context<S>)
        -> Result<PathBuf, DecodeError<S>>
    {
        match &**val {
            Literal::String(ref s) => Ok(String::from(s.clone()).into()),
            _ => {
                ctx.emit_error(DecodeError::scalar_kind(Kind::String, val));
                Ok(Default::default())
            }
        }
    }
    fn type_check(type_name: &Option<Spanned<TypeName, S>>,
                  ctx: &mut Context<S>)
    {
        if let Some(typ) = type_name {
            ctx.emit_error(DecodeError::TypeName {
                span: typ.span().clone(),
                found: Some(typ.value.clone()),
                expected: ExpectedType::no_type(),
                rust_type: "PathBuf",
            });
        }
    }
}

impl<S: ErrorSpan> DecodeScalar<S> for bool {
    fn raw_decode(val: &Spanned<Literal, S>, ctx: &mut Context<S>)
        -> Result<bool, DecodeError<S>>
    {
        match &**val {
            Literal::Bool(value) => Ok(*value),
            _ => {
                ctx.emit_error(DecodeError::scalar_kind(Kind::Bool, &val));
                Ok(Default::default())
            }
        }
    }
    fn type_check(type_name: &Option<Spanned<TypeName, S>>,
                  ctx: &mut Context<S>)
    {
        if let Some(typ) = type_name {
            ctx.emit_error(DecodeError::TypeName {
                span: typ.span().clone(),
                found: Some(typ.value.clone()),
                expected: ExpectedType::no_type(),
                rust_type: "bool",
            });
        }
    }
}
