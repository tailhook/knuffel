//! Structures that represent abstract syntax tree (AST) of the KDL document
//!
//! All of these types are parameterized by the `S` type which is a span type
//! (perhaps implements [`Span`](crate::traits::Span). The idea is that most of
//! the time spans are used for errors (either at parsing time, or at runtime),
//! and original source is somewhere around to show in error snippets. So it's
//! faster to only track byte offsets and calculate line number and column when
//! printing code snippet. So use [`span::Span`](crate::traits::Span).
//!
//! But sometimes you will not have KDL source around, or performance of
//! priting matters (i.e. you log source spans). In that case, span should
//! contain line and column numbers for things, use
//! [`LineSpan`](crate::span::LineSpan) for that.
use std::fmt;

use std::collections::BTreeMap;

use crate::span::Spanned;

/// A shortcut for nodes children that includes span of enclosing braces `{..}`
pub type SpannedChildren<S> = Spanned<Vec<SpannedNode<S>>, S>;
/// KDL names with span information are represented using this type
pub type SpannedName<S> = Spanned<Box<str>, S>;
/// A KDL node with span of the whole node (including children)
pub type SpannedNode<S> = Spanned<Node<S>, S>;

/// Single node of the KDL document
#[derive(Debug, Clone)]
pub struct Node<S> {
    /// A type name if specified in parenthesis
    pub type_name: Option<Spanned<TypeName, S>>,
    /// A node name
    pub node_name: SpannedName<S>,
    /// Positional arguments
    pub arguments: Vec<Value<S>>,
    /// Named properties
    pub properties: BTreeMap<SpannedName<S>, Value<S>>,
    /// Node's children. This field is not none if there are braces `{..}`
    pub children: Option<SpannedChildren<S>>,
}

/// KDL document root
#[derive(Debug, Clone)]
pub struct Document<S> {
    /// Nodes of the document
    pub nodes: Vec<SpannedNode<S>>,
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

/// Possibly typed KDL scalar value
#[derive(Debug, Clone)]
pub struct Value<S> {
    /// A type name if specified in parenthesis
    pub type_name: Option<Spanned<TypeName, S>>,
    /// The actual value literal
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

/// Known type identifier described by the KDL specification
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltinType {
    /// `u8`: 8-bit unsigned integer type
    U8,
    /// `i8`: 8-bit signed integer type
    I8,
    /// `u16`: 16-bit unsigned integer type
    U16,
    /// `i16`: 16-bit signed integer type
    I16,
    /// `u32`: 32-bit unsigned integer type
    U32,
    /// `i32`: 32-bit signed integer type
    I32,
    /// `u64`: 64-bit unsigned integer type
    U64,
    /// `i64`: 64-bit signed integer type
    I64,
    /// `base64` denotes binary bytes type encoded using base64 encoding
    Base64,
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
    /// Returns string representation of the builtin type as defined by KDL
    /// specification
    pub const fn as_str(&self) -> &'static str {
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
            Base64 => "base64",
        }
    }
    /// Returns `TypeName` structure for the builtin type
    pub const fn as_type(self) -> TypeName {
        TypeName(TypeNameInner::Builtin(self))
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
            "base64" => TypeName(Builtin(Base64)),
            _ => TypeName(Custom(val)),
        }
    }
    /// Returns string represenation of the type name
    pub fn as_str(&self) -> &str {
        match &self.0 {
            TypeNameInner::Builtin(t) => t.as_str(),
            TypeNameInner::Custom(t) => t.as_ref(),
        }
    }
    /// Returns `BuiltinType` enum for the type if typename matches builtin
    /// type
    ///
    /// Note: checking for `is_none()` is not forward compatible. In future we
    /// may add additional builtin type. Always use `as_str` for types that
    /// aren't yet builtin.
    pub const fn as_builtin(&self) -> Option<&BuiltinType> {
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

impl Into<TypeName> for BuiltinType {
    fn into(self) -> TypeName {
        self.as_type()
    }
}
