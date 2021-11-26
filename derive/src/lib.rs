use proc_macro2::TokenStream;

mod definition;
mod node;
mod kw;

use definition::Definition;


fn emit_decoder(def: &Definition) -> syn::Result<TokenStream> {
    match def {
        Definition::Struct(s) => node::emit_struct(s),
        Definition::TupleStruct(_) => todo!("decode tuple struct"),
        Definition::UnitStruct(_) => todo!("decode unit struct"),
        Definition::Enum(_) => todo!("decode enum"),
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
