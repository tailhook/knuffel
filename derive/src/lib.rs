use proc_macro2::TokenStream;

mod definition;
mod kw;
mod node;
mod variants;

use definition::Definition;


fn emit_decoder(def: &Definition) -> syn::Result<TokenStream> {
    match def {
        Definition::Struct(s) => node::emit_struct(s, true),
        Definition::TupleStruct(_) => todo!("decode tuple struct"),
        Definition::UnitStruct(s) => node::emit_struct(s, true),
        Definition::Enum(e) => variants::emit_enum(e),
    }
}

#[proc_macro_derive(Decode, attributes(knuffel))]
pub fn decode_derive(input: proc_macro::TokenStream)
    -> proc_macro::TokenStream
{
    let item = syn::parse_macro_input!(input as Definition);
    match emit_decoder(&item) {
        Ok(stream) => stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}
