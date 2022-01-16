use proc_macro2::TokenStream;

mod definition;
mod kw;
mod node;
mod scalar;
mod variants;

use definition::Definition;
use scalar::{Scalar, emit_scalar};


fn emit_decoder(def: &Definition) -> syn::Result<TokenStream> {
    match def {
        Definition::Struct(s) => node::emit_struct(s, true),
        Definition::NewType(s) => node::emit_new_type(s),
        Definition::TupleStruct(s) => node::emit_struct(s, false),
        Definition::UnitStruct(s) => node::emit_struct(s, true),
        Definition::Enum(e) => variants::emit_enum(e),
    }
}

#[proc_macro_derive(Decode, attributes(knuffel))]
#[doc = include_str!("../../derive_decode.md")]
pub fn decode_derive(input: proc_macro::TokenStream)
    -> proc_macro::TokenStream
{
    let item = syn::parse_macro_input!(input as Definition);
    match emit_decoder(&item) {
        Ok(stream) => stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

#[proc_macro_derive(DecodeScalar, attributes(knuffel))]
pub fn decode_scalar_derive(input: proc_macro::TokenStream)
    -> proc_macro::TokenStream
{
    let item = syn::parse_macro_input!(input as Scalar);
    match emit_scalar(&item) {
        Ok(stream) => stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}
