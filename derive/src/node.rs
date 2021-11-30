use proc_macro2::{TokenStream, Span};
use quote::quote;

use crate::definition::{Struct, ArgKind};


pub fn emit_struct(s: &Struct) -> syn::Result<TokenStream> {
    let name = &s.ident;
    let node = syn::Ident::new("node", Span::mixed_site());
    let children = syn::Ident::new("children", Span::mixed_site());
    let decode_args = decode_args(s, &node)?;
    let decode_props = decode_props(s, &node)?;
    let decode_children = decode_children(s, &children)?;
    let fields = s.all_fields();
    if s.children_only {
        Ok(quote! {
            impl<S: ::knuffel::traits::Span> ::knuffel::Decode<S> for #name {
                fn decode_node(#node: &::knuffel::ast::SpannedNode<S>)
                    -> Result<Self, ::knuffel::Error<S>>
                {
                    #decode_args  // these are basically assertions
                    #decode_props // these are basically assertions
                    let #children = #node.children.as_ref()
                        .map(|lst| &lst[..]).unwrap_or(&[]);
                    Self::decode_children(#children)
                }
                fn decode_children(#children: &[::knuffel::ast::SpannedNode<S>])
                    -> Result<Self, ::knuffel::Error<S>>
                {
                    #decode_children
                    Ok(#name {
                        #(#fields,)*
                    })
                }
            }
        })
    } else {
        let err = format!("bare set of children cannot be parsed into {} \
                           as it has arguments and properties", name);
        Ok(quote! {
            impl<S: ::knuffel::traits::Span> ::knuffel::Decode<S> for #name {
                fn decode_node(#node: &::knuffel::ast::SpannedNode<S>)
                    -> Result<Self, ::knuffel::Error<S>>
                {
                    #decode_args
                    #decode_props
                    let #children = #node.children.as_ref()
                        .map(|lst| &lst[..]).unwrap_or(&[]);
                    #decode_children
                    Ok(#name {
                        #(#fields,)*
                    })
                }
                fn decode_children(_nodes: &[::knuffel::ast::SpannedNode<S>])
                    -> Result<Self, ::knuffel::Error<S>>
                {
                    Err(::knuffel::Error::new_global(#err))
                }
            }
        })
    }
}

fn decode_args(s: &Struct, node: &syn::Ident) -> syn::Result<TokenStream> {
    let mut decoder = Vec::new();
    let iter_args = syn::Ident::new("iter_args", Span::mixed_site());
    decoder.push(quote! {
        let mut #iter_args = #node.arguments.iter();
    });
    for arg in &s.arguments {
        let fld = &arg.field;
        match arg.kind {
            ArgKind::Value { option: true } => {
                decoder.push(quote! {
                    let #fld = #iter_args.next()
                        .map(|v| v.try_into()).transpose()?;
                });
            }
            ArgKind::Value { option: false } => {
                let error = format!("additional argument `{}` is required",
                                    fld);
                decoder.push(quote! {
                    let #fld = #iter_args.next().ok_or_else(|| {
                        ::knuffel::Error::new(#node.node_name.span(), #error)
                    })?.try_into()?;
                });
            }
        }
    }
    if let Some(var_args) = &s.var_args {
        let fld = &var_args.field;
        decoder.push(quote! {
            let #fld = #iter_args.map(|v| v.try_into())
                .collect::<Result<_, _>>()?;
        });
    } else {
        decoder.push(quote! {
            if let Some(val) = #iter_args.next() {
                return Err(::knuffel::Error::new(val.literal.span(),
                                                 "unexpected argument"));
            }
        });
    }
    Ok(quote! { #(#decoder)* })
}

fn decode_props(s: &Struct, node: &syn::Ident) -> syn::Result<TokenStream> {
    let mut declare_empty = Vec::new();
    let mut match_branches = Vec::new();
    let mut postprocess = Vec::new();

    let val = syn::Ident::new("val", Span::mixed_site());
    let name = syn::Ident::new("name", Span::mixed_site());
    let name_str = syn::Ident::new("name_str", Span::mixed_site());

    for prop in &s.properties {
        let fld = &prop.field;
        let prop_name = prop.name();
        declare_empty.push(quote! {
            let mut #fld = None;
        });
        match_branches.push(quote! {
            #prop_name => #fld = Some(#val.try_into()?),
        });
        let req_msg = format!("property `{}` is required", prop_name);
        if !prop.option {
            postprocess.push(quote! {
                let #fld = #fld.ok_or_else(|| {
                    ::knuffel::Error::new(#node.node_name.span(), #req_msg)
                })?;
            });
        }
    }
    if let Some(var_props) = &s.var_props {
        let fld = &var_props.field;
        declare_empty.push(quote! {
            let mut #fld = Vec::new();
        });
        match_branches.push(quote! {
            #name_str => {
                let converted_name = #name_str.parse()
                    .map_err(|e| ::knuffel::Error::from_err(#name.span(), e))?;
                #fld.push((converted_name, #val.try_into()?));
            }
        });
        postprocess.push(quote! {
            let #fld = #fld.into_iter().collect();
        });
    } else {
        match_branches.push(quote! {
            #name_str => {
                return Err(::knuffel::Error::new(#name.span(),
                    format!("unexpected property `{}`",
                            #name_str.escape_default())));
            }
        });
    };
    Ok(quote! {
        #(#declare_empty)*
        for (#name, #val) in #node.properties.iter() {
            match &***#name {
                #(#match_branches)*
            }
        }
        #(#postprocess)*
    })
}

fn decode_children(s: &Struct, children: &syn::Ident)
    -> syn::Result<TokenStream>
{
    let mut decoder = Vec::new();
    let iter_children = syn::Ident::new("iter_children", Span::mixed_site());
    decoder.push(quote! {
        let mut #iter_children = #children.iter();
    });
    if let Some(children) = &s.children {
        let fld = &children.field;
        decoder.push(quote! {
            let #fld = #iter_children
                .map(|v| ::knuffel::Decode::decode_node(v))
                .collect::<Result<_, _>>()?;
        });
    } else {
        decoder.push(quote! {
            if let Some(val) = #iter_children.next() {
                return Err(::knuffel::Error::new(val.span(),
                                                 "unexpected child node"));
            }
        });
    }
    Ok(quote! { #(#decoder)* })
}
