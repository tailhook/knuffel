use proc_macro2::{TokenStream, Span};
use quote::quote;

use crate::definition::Struct;


pub fn emit_struct(s: &Struct) -> syn::Result<TokenStream> {
    let name = &s.ident;
    let node = syn::Ident::new("node", Span::mixed_site());
    let iter_args = syn::Ident::new("iter_args", Span::mixed_site());
    let decode_args = decode_args(s, &node, &iter_args)?;
    let fields = s.all_fields();
    Ok(quote! {
        impl<S: ::knuffel::traits::Span> ::knuffel::Decode<S> for #name {
            fn decode_node(#node: &::knuffel::ast::SpannedNode<S>)
                -> Result<Self, ::knuffel::Error<S>>
            {
                let mut #iter_args = #node.arguments.iter();
                #decode_args
                Ok(#name {
                    #(#fields,)*
                })
            }
            fn decode_children(_nodes: &[::knuffel::ast::SpannedNode<S>])
                -> Result<Self, ::knuffel::Error<S>>
            {
                todo!("decode children for Struct");
            }
        }
    })
}

pub fn decode_args(s: &Struct, node: &syn::Ident, iter_args: &syn::Ident)
    -> syn::Result<TokenStream>
{
    let mut decoder = Vec::new();
    for arg in &s.arguments {
        let fld = &arg.field;
        let error = format!("additional argument `{}` is required", fld);
        decoder.push(quote! {
            let #fld = #iter_args.next().ok_or_else(|| {
                ::knuffel::Error::new(#node.node_name.span(), #error)
            })?.try_into()?;
        });
    }
    decoder.push(quote! {
        if let Some(val) = #iter_args.next() {
            return Err(::knuffel::Error::new(val.literal.span(),
                                             "unexpected argument"));
        }
    });
    Ok(quote! { #(#decoder)* })
}
