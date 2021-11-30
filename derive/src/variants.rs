use proc_macro2::{TokenStream, Span};
use quote::quote;

use crate::definition::{Enum};


pub fn emit_enum(e: &Enum) -> syn::Result<TokenStream> {
    let name = &e.ident;
    let node = syn::Ident::new("node", Span::mixed_site());
    let decode = decode(e, &node)?;
    let err = format!("set of children cannot be parsed into enum {}", name);
    Ok(quote! {
        impl<S: ::knuffel::traits::Span> ::knuffel::Decode<S> for #name {
            fn decode_node(#node: &::knuffel::ast::SpannedNode<S>)
                -> Result<Self, ::knuffel::Error<S>>
            {
                #decode
            }
            fn decode_children(_nodes: &[::knuffel::ast::SpannedNode<S>])
                -> Result<Self, ::knuffel::Error<S>>
            {
                Err(::knuffel::Error::new_global(#err))
            }
        }
    })
}

fn decode(e: &Enum, node: &syn::Ident)
    -> syn::Result<TokenStream>
{
    let mut branches = Vec::with_capacity(e.variants.len());
    let enum_name = &e.ident;
    for var in &e.variants {
        let name = &var.name;
        let variant_name = &var.ident;
        branches.push(quote! {
            #name => ::knuffel::Decode::decode_node(#node)
                .map(#enum_name::#variant_name),
        });
    }
    // TODO(tailhook) use strsim to find similar names
    let err = if e.variants.len() <= 3 {
        format!("expected one of {}",
                e.variants.iter()
                .map(|v| format!("`{}`", v.name.escape_default()))
                .collect::<Vec<_>>()
                .join(", "))
    } else {
        format!("expected `{}`, `{}`, or one of {} others",
                e.variants[0].name.escape_default(),
                e.variants[1].name.escape_default(),
                e.variants.len() - 2)
    };
    Ok(quote! {
        match &**#node.node_name {
            #(#branches)*
            name_str => {
                Err(::knuffel::Error::new(#node.node_name.span(), #err))
            }
        }
    })
}
