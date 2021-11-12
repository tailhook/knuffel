use std::borrow::Cow;
use std::collections::BTreeMap;

use crate::span::Spanned;


/// Single node of the KDL document
pub struct Node<S> {
    pub type_name: Option<Spanned<TypeName, S>>,
    pub node_name: Spanned<Box<str>, S>,
    pub arguments: Vec<Spanned<Value<S>, S>>,
    pub properties: BTreeMap<Spanned<Box<str>, S>, Value<S>>,
    pub children: Option<Spanned<Vec<Spanned<Node<S>, S>>, S>>,
}

/// KDL document root
pub struct Document<S> {
    pub children: Vec<Spanned<Node<S>, S>>,
}

/// Potentially unlimited size integer value
pub struct Integer(Box<str>);

/// Potentially unlimited precision decimal value
pub struct Decimal(Box<str>);

/// Possibly typed value
pub struct Value<S> {
    pub type_name: Option<Spanned<TypeName, S>>,
    pub literal: Spanned<Literal, S>,
}

/// Type identifier
pub struct TypeName(Option<BuiltinType>, Cow<'static, str>);

/// Known type identifier described by specification
#[non_exhaustive]
pub enum BuiltinType {
}

/// Scalar KDL value
pub enum Literal {
    /// Integer value
    Int(Integer),
    /// Decimal (or floating point) value
    Decimal(Decimal),
    /// String value
    String(Box<str>),
    /// Boolean value
    Boolean(bool),
    /// Null value
    Null,
}

impl<S> Node<S> {
    /// Returns node children
    pub fn children(&self) -> impl Iterator<Item=&Spanned<Node<S>, S>> {
        self.children.iter().flat_map(|c| c.iter())
    }
}

