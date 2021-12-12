use std::default::Default;

use proc_macro2::{TokenStream, Span};
use quote::quote;

use crate::definition::{Struct, StructBuilder, ArgKind, FieldAttrs, DecodeMode};


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
                        .map_err(|e| e.ensure_span(#node.span()))
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

fn decode_value(val: &syn::Ident, mode: &DecodeMode)
    -> syn::Result<TokenStream>
{
    match mode {
        DecodeMode::Normal => {
            Ok(quote!{
                ::knuffel::traits::DecodeScalar::decode(#val)
            })
        }
        DecodeMode::Str => {
            Ok(quote! {
                if let Some(typ) = &#val.type_name {
                    Err(::knuffel::Error::new(typ.span(),
                        format!("no type name expected")))
                } else {
                    match *#val.literal {
                        ::knuffel::ast::Literal::String(ref s) => {
                            ::std::str::FromStr::from_str(s)
                                .map_err(|e| ::knuffel::Error::from_err(
                                    #val.literal.span(), e))
                        }
                        _ => Err(::knuffel::Error::new(#val.literal.span(),
                                            "expected string value")),
                    }
                }
            })
        }
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
        let val = syn::Ident::new("val", Span::mixed_site());
        let decode_value = decode_value(&val, &arg.decode)?;
        match arg.kind {
            ArgKind::Value { option: true } => {
                decoder.push(quote! {
                    let #fld = #iter_args.next().map(|#val| {
                        #decode_value
                    }).transpose()?;
                });
            }
            ArgKind::Value { option: false } => {
                let error = format!("additional argument `{}` is required",
                                    fld);
                decoder.push(quote! {
                    let #val =
                        #iter_args.next().ok_or_else(|| {
                            ::knuffel::Error::new(
                                #node.node_name.span(), #error)
                        })?;
                    let #fld = #decode_value?;
                });
            }
        }
    }
    if let Some(var_args) = &s.var_args {
        let fld = &var_args.field;
        let val = syn::Ident::new("val", Span::mixed_site());
        let decode_value = decode_value(&val, &var_args.decode)?;
        decoder.push(quote! {
            let #fld = #iter_args.map(|#val| {
                #decode_value
            }).collect::<Result<_, _>>()?;
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
        let decode_value = decode_value(&val, &prop.decode)?;
        declare_empty.push(quote! {
            let mut #fld = None;
        });
        match_branches.push(quote! {
            #prop_name => #fld = Some(#decode_value?),
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
        let decode_value = decode_value(&val, &var_props.decode)?;
        declare_empty.push(quote! {
            let mut #fld = Vec::new();
        });
        match_branches.push(quote! {
            #name_str => {
                let converted_name = #name_str.parse()
                    .map_err(|e| ::knuffel::Error::from_err(#name.span(), e))?;
                #fld.push((
                    converted_name,
                    #decode_value?,
                ));
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

fn unwrap_fn(func: &syn::Ident,
             name: &syn::Ident, is_option: bool, attrs: &FieldAttrs)
    -> syn::Result<TokenStream>
{
    let mut bld = StructBuilder::new(
        syn::Ident::new(&format!("Wrap_{}", name), Span::mixed_site()),
        Default::default(),
    );
    bld.add_field(name.clone(), is_option, attrs)?;
    let s = bld.build();

    let node = syn::Ident::new("node", Span::mixed_site());
    let children = syn::Ident::new("children", Span::mixed_site());
    let decode_args = decode_args(&s, &node)?;
    let decode_props = decode_props(&s, &node)?;
    let decode_children = decode_children(&s, &children)?;
    Ok(quote! {
        let #func = |#node: &::knuffel::ast::SpannedNode<S>| {
            #decode_args
            #decode_props
            let #children = #node.children.as_ref()
                .map(|lst| &lst[..]).unwrap_or(&[]);
            #decode_children

            Ok(#name)
        };
    })
}

fn decode_children(s: &Struct, children: &syn::Ident)
    -> syn::Result<TokenStream>
{
    let mut declare_empty = Vec::new();
    let mut match_branches = Vec::new();
    let mut postprocess = Vec::new();

    let child = syn::Ident::new("child", Span::mixed_site());
    let name_str = syn::Ident::new("name_str", Span::mixed_site());

    for child_def in &s.children {
        let fld = &child_def.field;
        let child_name = &child_def.name;
        declare_empty.push(quote! {
            let mut #fld = None;
        });
        let dup_err = format!("duplicate node `{}`, single node expected",
                              child_name.escape_default());
        let decode = if let Some(inner) = child_def.unwrap.as_ref() {
            let func = syn::Ident::new(&format!("unwrap_{}", fld),
                                       Span::mixed_site());
            let unwrap_fn = unwrap_fn(&func, fld, child_def.option, inner)?;
            quote! {
                #unwrap_fn
                match #func(#child) {
                    Ok(#child) => {
                        #fld = Some(#child);
                        None
                    }
                    Err(e) => Some(Err(e)),
                }
            }
        } else {
            quote! {
                match ::knuffel::Decode::decode_node(#child) {
                    Ok(#child) => {
                        #fld = Some(#child);
                        None
                    }
                    Err(e) => Some(Err(e)),
                }
            }
        };
        match_branches.push(quote! {
            #child_name => {
                if #fld.is_some() {
                    Some(Err(::knuffel::Error::new(#child.node_name.span(),
                                                   #dup_err)))
                } else {
                    #decode
                }
            }
        });
        let req_msg = format!("child node `{}` is required", child_name);
        if !child_def.option {
            postprocess.push(quote! {
                let #fld = #fld.ok_or_else(|| {
                    ::knuffel::Error::new_global(#req_msg)
                })?;
            });
        }
    }
    if let Some(var_children) = &s.var_children {
        let fld = &var_children.field;
        match_branches.push(quote! {
            _ => {
                match ::knuffel::Decode::decode_node(#child) {
                    Ok(#child) => Some(Ok(#child)),
                    Err(e) => Some(Err(e)),
                }
            }
        });
        Ok(quote! {
            #(#declare_empty)*
            let #fld = #children.iter().flat_map(|#child| {
                match &**#child.node_name {
                    #(#match_branches)*
                }
            }).collect::<Result<_, ::knuffel::Error<_>>>()?;
            #(#postprocess)*
        })
    } else {
        match_branches.push(quote! {
            #name_str => {
                return Some(Err(::knuffel::Error::new(#child.span(),
                    format!("unexpected node `{}`",
                            #name_str.escape_default()))));
            }
        });

        Ok(quote! {
            #(#declare_empty)*
            #children.iter().flat_map(|#child| {
                match &**#child.node_name {
                    #(#match_branches)*
                }
            }).collect::<Result<(), ::knuffel::Error<_>>>()?;
            #(#postprocess)*
        })
    }
}
