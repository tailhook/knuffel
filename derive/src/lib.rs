use proc_macro2::TokenStream;

mod definition;

use definition::Definition;


fn emit_decoder(_def: &Definition) -> syn::Result<TokenStream> {
    todo!();
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
