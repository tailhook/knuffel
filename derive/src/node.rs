use proc_macro2::{TokenStream, Span};
use quote::quote;

use crate::definition::Struct;


pub fn emit_struct(s: &Struct) -> syn::Result<TokenStream> {
    let name = &s.ident;
    let node = syn::Ident::new("node", Span::mixed_site());
    let decode_args = decode_args(s, &node)?;
    let decode_props = decode_props(s, &node)?;
    let fields = s.all_fields();
    Ok(quote! {
        impl<S: ::knuffel::traits::Span> ::knuffel::Decode<S> for #name {
            fn decode_node(#node: &::knuffel::ast::SpannedNode<S>)
                -> Result<Self, ::knuffel::Error<S>>
            {
                #decode_args
                #decode_props
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

pub fn decode_args(s: &Struct, node: &syn::Ident) -> syn::Result<TokenStream> {
    let mut decoder = Vec::new();
    let iter_args = syn::Ident::new("iter_args", Span::mixed_site());
    decoder.push(quote! {
        let mut #iter_args = #node.arguments.iter();
    });
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

pub fn decode_props(s: &Struct, node: &syn::Ident) -> syn::Result<TokenStream> {
    let mut declare_nones = Vec::new();
    let mut match_branches = Vec::new();
    let mut unwrap_required = Vec::new();
    let val = syn::Ident::new("val", Span::mixed_site());
    for prop in &s.properties {
        let fld = &prop.field;
        let name = prop.name();
        declare_nones.push(quote! {
            let mut #fld = None;
        });
        match_branches.push(quote! {
            #name => #fld = Some(#val.try_into()?),
        });
        let req_msg = format!("property `{}` is required", prop.name());
        unwrap_required.push(quote! {
            let #fld = #fld.ok_or_else(|| {
                ::knuffel::Error::new(#node.node_name.span(), #req_msg)
            })?;
        });
    }
    let name = syn::Ident::new("name", Span::mixed_site());
    Ok(quote! {
        #(#declare_nones)*
        for (#name, #val) in #node.properties.iter() {
            match &***#name {
                #(#match_branches)*
                name => {
                    return Err(::knuffel::Error::new(#name.span(),
                        format!("unexpected property `{}`",
                                name.escape_default())));
                }
            }
        }
        #(#unwrap_required)*
    })
}
