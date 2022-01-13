use proc_macro2::{TokenStream, Span};
use quote::quote;

use crate::definition::{Enum, VariantKind};
use crate::node;


pub fn emit_enum(e: &Enum) -> syn::Result<TokenStream> {
    let name = &e.ident;
    let node = syn::Ident::new("node", Span::mixed_site());
    let ctx = syn::Ident::new("ctx", Span::mixed_site());
    let decode = decode(e, &node, &ctx)?;
    Ok(quote! {
        impl<S: ::knuffel::traits::Span> ::knuffel::Decode<S> for #name {
            fn decode_node(#node: &::knuffel::ast::SpannedNode<S>,
                           #ctx: &mut ::knuffel::decode::Context<S>)
                -> Result<Self, ::knuffel::errors::DecodeError<S>>
            {
                #decode
            }
        }
    })
}

fn decode(e: &Enum, node: &syn::Ident, ctx: &syn::Ident)
    -> syn::Result<TokenStream>
{
    let mut branches = Vec::with_capacity(e.variants.len());
    let enum_name = &e.ident;
    for var in &e.variants {
        let name = &var.name;
        let variant_name = &var.ident;
        match &var.kind {
            VariantKind::Unit => {
                branches.push(quote! {
                    #name => {
                        for arg in &#node.arguments {
                            #ctx.emit_error(
                                ::knuffel::errors::DecodeError::unexpected(
                                    &arg.literal, "argument",
                                    "unexpected argument"));
                        }
                        for (name, _) in &#node.properties {
                            #ctx.emit_error(
                                ::knuffel::errors::DecodeError::unexpected(
                                    name, "property",
                                    format!("unexpected property `{}`",
                                            name.escape_default())));
                        }
                        if let Some(children) = &#node.children {
                            for child in children.iter() {
                                #ctx.emit_error(
                                    ::knuffel::errors::DecodeError::unexpected(
                                        child, "node",
                                        format!("unexpected node `{}`",
                                            child.node_name.escape_default())
                                    ));
                            }
                        }
                        Ok(#enum_name::#variant_name)
                    }
                });
            }
            VariantKind::Nested { option: false } => {
                branches.push(quote! {
                    #name => ::knuffel::Decode::decode_node(#node, #ctx)
                        .map(#enum_name::#variant_name),
                });
            }
            VariantKind::Nested { option: true } => {
                branches.push(quote! {
                    #name => {
                        if #node.arguments.len() > 0 ||
                            #node.properties.len() > 0 ||
                            #node.children.is_some()
                        {
                            ::knuffel::Decode::decode_node(#node, #ctx)
                                .map(Some)
                                .map(#enum_name::#variant_name)
                        } else {
                            Ok(#enum_name::#variant_name(None))
                        }
                    }
                });
            }
            VariantKind::Tuple(s) => {
                let decode = node::decode_enum_item(s,
                    quote!(#enum_name::#variant_name),
                    node, ctx, false)?;
                branches.push(quote! {
                    #name => { #decode }
                });
            }
            VariantKind::Named(_) => todo!(),
        }
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
                Err(::knuffel::errors::DecodeError::conversion(
                        &#node.node_name, #err))
            }
        }
    })
}
