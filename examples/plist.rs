/*!
<https://www.apple.com/DTDs/PropertyList-1.0.dtd>
*/

use chrono::NaiveDateTime;
use knuffel::Decode;

#[derive(Debug, Decode)]
pub struct PList {
    #[knuffel(property)]
    version: String,
    #[knuffel(children)]
    elements: Vec<Element>
}

#[derive(Debug, Decode)]
pub enum Element {
    Array(#[knuffel(children)] Vec<Box<Element>>),
    Data(#[knuffel(argument, bytes)] Vec<u8>),
    Date(#[knuffel(argument, str)] NaiveDateTime),
    Dict(Box<Dict>),
    Real(#[knuffel(argument)] i32),  // TODO f32
    Integer(#[knuffel(argument)] i32),
    String(#[knuffel(argument)] String),
    True,
    False
}

#[derive(Debug, Decode)]
pub struct Dict {
    #[knuffel(children(name = "key"))]
    keys: Vec<Key>,
    #[knuffel(children)]
    values: Vec<Element>
}

#[derive(Debug, Decode)]
pub struct Key(#[knuffel(argument)] String);
