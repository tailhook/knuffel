use std::borrow::Cow;
use std::collections::BTreeSet;

use crate::span::Spanned;


/// Single node of the KDL document
pub struct Node<S> {
    type_name: Option<Spanned<Cow<'static, str>, S>>,
    node_name: Spanned<String, S>,
    arguments: Vec<Spanned<Value, S>>,
    properties: BTreeSet<Property<S>>,
    children: Option<Spanned<Vec<Spanned<Node<S>, S>>, S>>,
}

/// KDL document root
pub struct Document<S> {
    children: Vec<Spanned<Node<S>, S>>,
}

/// Potentially unlimited size integer value
pub struct Integer(String);

/// Potentially unlimited precision decimal value
pub struct Decimal(String);

/// Scalar KDL value
pub enum Value {
    /// Integer value
    Int(Integer),
    /// Floating point value
    Float(Decimal),
    /// String value
    String(String),
    /// Boolean value
    Boolean(bool),
    /// Null value
    Null,
}

struct Property<S> {
    span: S,
    name: String,
    value: Value,
}

impl<S> Node<S> {
    /// Returns node type name if specified
    pub fn type_name(&self) -> Option<&str>{
        self.type_name.as_ref().map(|x| &x[..])
    }
    /// Returns node name if specified
    pub fn node_name(&self) -> &str {
        &self.node_name[..]
    }
    /// Returns node argument values
    pub fn arguments(&self) -> impl Iterator<Item=&Value> {
        self.arguments.iter().map(|x| &**x)
    }
    /// Returns node properties
    pub fn properties(&self) -> impl Iterator<Item=(&str, &Value)> {
        self.properties.iter().map(|p| (&p.name[..], &p.value))
    }

    /// Returns node children
    pub fn children(&self) -> impl Iterator<Item=&Node<S>> {
        self.children.iter().flat_map(|c| c.iter()).map(|c| &**c)
    }

    /// Returns source span of the type name if specified
    pub fn type_name_span(&self) -> Option<&S> {
        self.type_name.as_ref().map(|x| x.span())
    }
    /// Returns source span of the node name
    pub fn node_name_span(&self) -> &S {
        self.node_name.span()
    }
    /// Returns each argument with it's own source span
    pub fn spanned_arguments(&self) -> impl Iterator<Item=(&S, &Value)> {
        self.arguments.iter().map(|s| (s.span(), &**s))
    }
    /// Returns each property with the span of whole key=value pair
    pub fn spanned_properties(&self) -> impl Iterator<Item=(&S, &str, &Value)>
    {
        self.properties.iter().map(|s| (&s.span, &s.name[..], &s.value))
    }
    /// Returns each argument with it's own source span
    pub fn spanned_children(&self) -> impl Iterator<Item=(&S, &Node<S>)> {
        self.children.iter().flat_map(|c| c.iter()).map(|s| (s.span(), &**s))
    }
    /// Returns the span of the children if curly braces were specified
    pub fn children_span(&self) -> Option<&S> {
        self.children.as_ref().map(|m| m.span())
    }
}

impl<S> Document<S> {
    /// Returns node children
    pub fn children(&self) -> impl Iterator<Item=&Node<S>> {
        self.children.iter().map(|c| &**c)
    }

    /// Returns each argument with it's own source span
    pub fn spanned_children(&self) -> impl Iterator<Item=(&S, &Node<S>)> {
        self.children.iter().map(|s| (s.span(), &**s))
    }
}

impl<S> PartialEq for Property<S> {
    fn eq(&self, other: &Property<S>) -> bool {
        self.name == other.name
    }
}

impl<S> PartialOrd for Property<S> {
    fn partial_cmp(&self, other: &Property<S>) -> Option<std::cmp::Ordering> {
        self.name.partial_cmp(&other.name)
    }
}

impl<S> Ord for Property<S> {
    fn cmp(&self, other: &Property<S>) -> std::cmp::Ordering {
        self.name.cmp(&other.name)
    }
}

impl<S> Eq for Property<S> {}
