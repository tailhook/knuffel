use std::sync::Arc;
use std::rc::Rc;

use crate::ast::{SpannedNode, Literal, Value, TypeName};
use crate::errors::Error;
use crate::span::Spanned;
use crate::traits::{Decode, DecodeChildren, DecodeScalar, DecodePartial};


impl<S, T: Decode<S>> Decode<S> for Box<T> {
    fn decode_node(node: &SpannedNode<S>) -> Result<Self, Error<S>> {
        Decode::decode_node(node).map(Box::new)
    }
}

impl<S, T: DecodeChildren<S>> DecodeChildren<S> for Box<T> {
    fn decode_children(nodes: &[SpannedNode<S>]) -> Result<Self, Error<S>> {
        DecodeChildren::decode_children(nodes).map(Box::new)
    }
}

impl<S, T: DecodePartial<S>> DecodePartial<S> for Box<T> {
    fn insert_child(&mut self, node: &SpannedNode<S>) -> Result<bool, Error<S>>
    {
        (**self).insert_child(node)
    }
    fn insert_property(&mut self, name: &Spanned<Box<str>, S>, value: &Value<S>)
        -> Result<bool, Error<S>>
    {
        (**self).insert_property(name, value)
    }
}

impl<S, T: DecodeScalar<S>> DecodeScalar<S> for Box<T> {
    fn type_check(type_name: &Option<Spanned<TypeName, S>>)
        -> Result<(), Error<S>>
    {
        T::type_check(type_name)
    }
    fn raw_decode(value: &Spanned<Literal, S>) -> Result<Self, Error<S>> {
        DecodeScalar::raw_decode(value).map(Box::new)
    }
}

impl<S, T: Decode<S>> Decode<S> for Arc<T> {
    fn decode_node(node: &SpannedNode<S>) -> Result<Self, Error<S>> {
        Decode::decode_node(node).map(Arc::new)
    }
}

impl<S, T: DecodeChildren<S>> DecodeChildren<S> for Arc<T> {
    fn decode_children(nodes: &[SpannedNode<S>]) -> Result<Self, Error<S>> {
        DecodeChildren::decode_children(nodes).map(Arc::new)
    }
}

impl<S, T: DecodePartial<S>> DecodePartial<S> for Arc<T> {
    fn insert_child(&mut self, node: &SpannedNode<S>) -> Result<bool, Error<S>>
    {
        Arc::get_mut(self).expect("no Arc clone yet")
            .insert_child(node)
    }
    fn insert_property(&mut self, name: &Spanned<Box<str>, S>, value: &Value<S>)
        -> Result<bool, Error<S>>
    {
        Arc::get_mut(self).expect("no Arc clone yet")
            .insert_property(name, value)
    }
}

impl<S, T: DecodeScalar<S>> DecodeScalar<S> for Arc<T> {
    fn type_check(type_name: &Option<Spanned<TypeName, S>>)
        -> Result<(), Error<S>>
    {
        T::type_check(type_name)
    }
    fn raw_decode(value: &Spanned<Literal, S>) -> Result<Self, Error<S>> {
        DecodeScalar::raw_decode(value).map(Arc::new)
    }
}

impl<S, T: Decode<S>> Decode<S> for Rc<T> {
    fn decode_node(node: &SpannedNode<S>) -> Result<Self, Error<S>> {
        Decode::decode_node(node).map(Rc::new)
    }
}

impl<S, T: DecodeChildren<S>> DecodeChildren<S> for Rc<T> {
    fn decode_children(nodes: &[SpannedNode<S>]) -> Result<Self, Error<S>> {
        DecodeChildren::decode_children(nodes).map(Rc::new)
    }
}

impl<S, T: DecodePartial<S>> DecodePartial<S> for Rc<T> {
    fn insert_child(&mut self, node: &SpannedNode<S>) -> Result<bool, Error<S>>
    {
        Rc::get_mut(self).expect("no Rc clone yet")
            .insert_child(node)
    }
    fn insert_property(&mut self, name: &Spanned<Box<str>, S>, value: &Value<S>)
        -> Result<bool, Error<S>>
    {
        Rc::get_mut(self).expect("no Rc clone yet")
            .insert_property(name, value)
    }
}

impl<S, T: DecodeScalar<S>> DecodeScalar<S> for Rc<T> {
    fn type_check(type_name: &Option<Spanned<TypeName, S>>)
        -> Result<(), Error<S>>
    {
        T::type_check(type_name)
    }
    fn raw_decode(value: &Spanned<Literal, S>) -> Result<Self, Error<S>> {
        DecodeScalar::raw_decode(value).map(Rc::new)
    }
}
