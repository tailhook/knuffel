use std::fmt;

use std::collections::BTreeMap;

use crate::span::Spanned;

pub type SpannedChildren<S> = Spanned<Vec<SpannedNode<S>>, S>;
pub type SpannedName<S> = Spanned<Box<str>, S>;
pub type SpannedNode<S> = Spanned<Node<S>, S>;

/// Single node of the KDL document
#[derive(Debug, Clone)]
pub struct Node<S> {
    pub type_name: Option<Spanned<TypeName, S>>,
    pub node_name: Spanned<Box<str>, S>,
    pub arguments: Vec<Value<S>>,
    pub properties: BTreeMap<Spanned<Box<str>, S>, Value<S>>,
    pub children: Option<SpannedChildren<S>>,
}

/// KDL document root
#[derive(Debug, Clone)]
pub struct Document<S> {
    pub nodes: Vec<Spanned<Node<S>, S>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Radix {
    Bin,
    Hex,
    Oct,
    Dec,
}

/// Potentially unlimited size integer value
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Integer(pub(crate) Radix, pub(crate) Box<str>);

/// Potentially unlimited precision decimal value
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Decimal(pub(crate) Box<str>);

/// Possibly typed value
#[derive(Debug, Clone)]
pub struct Value<S> {
    pub type_name: Option<Spanned<TypeName, S>>,
    pub literal: Spanned<Literal, S>,
}

/// Type identifier
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeName(TypeNameInner);

#[derive(Debug, Clone, PartialEq, Eq)]
enum TypeNameInner {
    Builtin(BuiltinType),
    Custom(Box<str>),
}

/// Known type identifier described by specification
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltinType {
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
}

/// Scalar KDL value
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    /// Integer value
    Int(Integer),
    /// Decimal (or floating point) value
    Decimal(Decimal),
    /// String value
    String(Box<str>),
    /// Boolean value
    Bool(bool),
    /// Null value
    Null,
}

impl<S> Node<S> {
    /// Returns node children
    pub fn children(&self)
        -> impl Iterator<Item=&Spanned<Node<S>, S>> +
                ExactSizeIterator
    {
        self.children.as_ref().map(|c| c.iter()).unwrap_or_else(|| [].iter())
    }
}

impl BuiltinType {
    fn as_str(&self) -> &'static str {
        use BuiltinType::*;
        match self {
            U8 => "u8",
            I8 => "i8",
            U16 => "u16",
            I16 => "i16",
            U32 => "u32",
            I32 => "i32",
            U64 => "u64",
            I64 => "i64",
        }
    }
}

impl TypeName {
    // TODO(tailhook) for public API check identifier for validness
    pub(crate) fn from_string(val: Box<str>) -> TypeName {
        use BuiltinType::*;
        use TypeNameInner::*;

        match &val[..] {
            "u8" => TypeName(Builtin(U8)),
            "i8" => TypeName(Builtin(I8)),
            "u16" => TypeName(Builtin(U16)),
            "i16" => TypeName(Builtin(I16)),
            "u32" => TypeName(Builtin(U32)),
            "i32" => TypeName(Builtin(I32)),
            "u64" => TypeName(Builtin(U64)),
            "i64" => TypeName(Builtin(I64)),
            _ => TypeName(Custom(val)),
        }
    }
    pub fn as_str(&self) -> &str {
        match &self.0 {
            TypeNameInner::Builtin(t) => t.as_str(),
            TypeNameInner::Custom(t) => t.as_ref(),
        }
    }
    pub fn as_builtin(&self) -> Option<&BuiltinType> {
        match &self.0 {
            TypeNameInner::Builtin(t) => Some(t),
            TypeNameInner::Custom(_) => None,
        }
    }
}

impl std::ops::Deref for TypeName {
    type Target = str;
    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.as_str().fmt(f)
    }
}
