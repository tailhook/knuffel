use proc_macro2::{TokenStream, Span};
use quote::quote;

use crate::definition::{Enum, VariantKind};


pub fn emit_enum(e: &Enum) -> syn::Result<TokenStream> {
    let name = &e.ident;
    let node = syn::Ident::new("node", Span::mixed_site());
    let decode = decode(e, &node)?;
    Ok(quote! {
        impl<S: ::knuffel::traits::Span> ::knuffel::Decode<S> for #name {
            fn decode_node(#node: &::knuffel::ast::SpannedNode<S>)
                -> Result<Self, ::knuffel::Error<S>>
            {
                #decode
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
        match var.kind {
            VariantKind::Unit => {
                branches.push(quote! {
                    #name => {
                        for arg in &#node.arguments {
                            return Err(::knuffel::Error::new(
                                arg.literal.span(), "unexpected argument"));
                        }
                        for (name, _) in &#node.properties {
                            return Err(::knuffel::Error::new(name.span(),
                                format!("unexpected property `{}`",
                                        name.escape_default())));
                        }
                        if let Some(children) = &#node.children {
                            for child in children.iter() {
                                return Err(::knuffel::Error::new(child.span(),
                                    format!("unexpected node `{}`",
                                        child.node_name.escape_default())
                                ));
                            }
                        }
                        Ok(#enum_name::#variant_name)
                    }
                });
            }
            VariantKind::Nested => {
                branches.push(quote! {
                    #name => ::knuffel::Decode::decode_node(#node)
                        .map(#enum_name::#variant_name),
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
                Err(::knuffel::Error::new(#node.node_name.span(), #err))
            }
        }
    })
}
