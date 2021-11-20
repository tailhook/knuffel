use crate::ast::SpannedNode;
use crate::errors::Error;


pub trait Decode<S>: Sized {
    fn decode_node(node: &SpannedNode<S>) -> Result<Self, Error<S>>;
    fn decode_children(nodes: &[SpannedNode<S>]) -> Result<Self, Error<S>>;
}
