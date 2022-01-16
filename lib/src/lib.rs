#![doc = include_str!("../../README.md")]
#![warn(missing_docs)]
#![warn(missing_debug_implementations)]

mod containers;
mod convert;
mod grammar;
mod wrappers;

pub mod ast;
pub mod decode;
pub mod errors;
pub mod span;
pub mod traits;

#[cfg(feature="derive")]
pub use knuffel_derive::{Decode, DecodeScalar};

pub use wrappers::{parse_ast, parse, parse_with_context};
pub use traits::{Decode, DecodeScalar, DecodeChildren};
pub use errors::Error;
