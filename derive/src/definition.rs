use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use proc_macro2::Span;

use crate::kw;

pub enum Definition {
    UnitStruct(UnitStruct),
    TupleStruct(TupleStruct),
    Struct(Struct),
    Enum(Enum),
}

pub enum ArgKind {
    Value { option: bool },
}

#[derive(Debug, Clone)]
pub enum FieldMode {
    Argument,
    Property,
    Arguments,
    Properties,
    Children,
    Child,
    Flatten(Flatten),
}

pub enum FlattenItem {
    Child,
    Property,
}

#[derive(Debug, Clone)]
pub struct Flatten {
    child: bool,
    property: bool,
}

#[derive(Debug, Clone)]
pub enum DecodeMode {
    Normal,
    Str,
}

#[derive(Debug)]
pub enum Attr {
    Skip,
    DecodeMode(DecodeMode),
    FieldMode(FieldMode),
    Unwrap(FieldAttrs),
}

#[derive(Debug, Clone)]
pub struct FieldAttrs {
    pub mode: Option<FieldMode>,
    pub decode: Option<DecodeMode>,
    pub unwrap: Option<Box<FieldAttrs>>,
}

#[derive(Debug, Clone)]
pub struct VariantAttrs {
    pub skip: bool,
}

pub struct Arg {
    pub field: syn::Ident,
    pub kind: ArgKind,
    pub decode: DecodeMode,
}

pub struct VarArgs {
    pub field: syn::Ident,
    pub decode: DecodeMode,
}

pub struct Prop {
    pub field: syn::Ident,
    pub option: bool,
    pub decode: DecodeMode,
    pub flatten: bool,
}

pub struct VarProps {
    pub field: syn::Ident,
    pub decode: DecodeMode,
}

pub struct Child {
    pub field: syn::Ident,
    pub name: String,
    pub option: bool,
    pub unwrap: Option<FieldAttrs>,
    pub flatten: bool,
}

pub struct VarChildren {
    pub field: syn::Ident,
}

pub struct TupleArg {
    pub default: Option<syn::Expr>,
    pub kind: ArgKind,
}

pub enum ExtraKind {
    Default,
}

pub struct ExtraField {
    pub ident: syn::Ident,
    pub kind: ExtraKind,
}

pub struct UnitStruct {
    pub ident: syn::Ident,
    pub generics: syn::Generics,
}

pub struct TupleStruct {
    pub ident: syn::Ident,
    pub generics: syn::Generics,
    pub arguments: Vec<TupleArg>,
}


pub struct Struct {
    pub ident: syn::Ident,
    pub generics: syn::Generics,
    pub arguments: Vec<Arg>,
    pub var_args: Option<VarArgs>,
    pub properties: Vec<Prop>,
    pub var_props: Option<VarProps>,
    pub has_arguments: bool,
    pub has_properties: bool,
    pub children: Vec<Child>,
    pub var_children: Option<VarChildren>,
    pub extra_fields: Vec<ExtraField>,
}

pub struct StructBuilder {
    pub ident: syn::Ident,
    pub generics: syn::Generics,
    pub arguments: Vec<Arg>,
    pub var_args: Option<VarArgs>,
    pub properties: Vec<Prop>,
    pub var_props: Option<VarProps>,
    pub children: Vec<Child>,
    pub var_children: Option<VarChildren>,
    pub extra_fields: Vec<ExtraField>,
}

pub struct Variant {
    pub ident: syn::Ident,
    pub name: String,
}

pub struct Enum {
    pub ident: syn::Ident,
    pub generics: syn::Generics,
    pub variants: Vec<Variant>,
}


impl UnitStruct {
    fn new(ident: syn::Ident, generics: syn::Generics,
           _attrs: Vec<syn::Attribute>)
        -> syn::Result<Self>
    {
        // todo(tailhook) verify there are no attributes
        Ok(UnitStruct { ident, generics })
    }
}

impl TupleStruct {
    fn new(_ident: syn::Ident, _generics: syn::Generics,
           _attrs: Vec<syn::Attribute>,
           _fields: impl Iterator<Item=syn::Field>)
        -> syn::Result<Self>
    {
        todo!("TupleStruct constrcutor");
    }
}

fn err_pair(s1: impl quote::ToTokens, s2: impl quote::ToTokens,
            t1: &str, t2: &str)
    -> syn::Error
{
    let mut err = syn::Error::new_spanned(s1, t1);
    err.combine(syn::Error::new_spanned(s2, t2));
    return err;
}

fn is_option(ty: &syn::Type) -> bool {
    matches!(ty,
        syn::Type::Path(syn::TypePath {
            qself: None,
            path: syn::Path {
                leading_colon: None,
                segments,
            },
        })
        if segments.len() == 1 && segments[0].ident == "Option"
    )
}

impl Variant {
    fn new(ident: syn::Ident, _attrs: VariantAttrs) -> syn::Result<Self>
    {
        let name = heck::KebabCase::to_kebab_case(&ident.to_string()[..]);
        Ok(Variant {
            ident,
            name,
        })
    }
}

impl Enum {
    fn new(ident: syn::Ident, generics: syn::Generics,
           _attrs: Vec<syn::Attribute>,
           src_variants: impl Iterator<Item=syn::Variant>)
        -> syn::Result<Self>
    {
        let mut variants = Vec::new();
        for var in src_variants {
            let mut attrs = VariantAttrs::new();
            for attr in var.attrs {
                if matches!(attr.style, syn::AttrStyle::Outer) &&
                    attr.path.is_ident("knuffel")

                {
                    let chunk = attr.parse_args_with(parse_attrs)?;
                    attrs.update(chunk)?;
                }
            }
            if attrs.skip {
                continue;
            }
            match var.fields {
                syn::Fields::Named(n) => {
                    return Err(syn::Error::new_spanned(n,
                        "named fields are not supported in enum variants"));
                }
                syn::Fields::Unnamed(u) => {
                    if u.unnamed.len() != 1 {
                        return Err(syn::Error::new_spanned(u,
                            "single field expected"));
                    }
                    variants.push(Variant::new(var.ident, attrs)?);
                }
                syn::Fields::Unit => {
                    return Err(syn::Error::new_spanned(var.ident,
                        "unit variants are not supported in enum variants"));
                }
            }
        }
        Ok(Enum {
            ident,
            generics,
            variants,
        })
    }
}

impl StructBuilder {
    pub fn new(ident: syn::Ident, generics: syn::Generics) -> Self {
        StructBuilder {
            ident,
            generics,
            arguments: Vec::new(),
            var_args: None::<VarArgs>,
            properties: Vec::new(),
            var_props: None::<VarProps>,
            children: Vec::new(),
            var_children: None::<VarChildren>,
            extra_fields: Vec::new(),
        }
    }
    pub fn build(self) -> Struct {
        Struct {
            ident: self.ident,
            generics: self.generics,
            has_arguments:
                !self.arguments.is_empty() || self.var_args.is_some(),
            has_properties:
                !self.properties.is_empty() || self.var_props.is_some(),
            arguments: self.arguments,
            var_args: self.var_args,
            properties: self.properties,
            var_props: self.var_props,
            children: self.children,
            var_children: self.var_children,
            extra_fields: self.extra_fields,
        }
    }
    pub fn add_field(&mut self, ident: syn::Ident, is_option: bool,
                     attrs: &FieldAttrs)
        -> syn::Result<&mut Self>
    {
        match &attrs.mode {
            Some(FieldMode::Argument) => {
                if let Some(prev) = &self.var_args {
                    return Err(err_pair(ident, &prev.field,
                        "extra `argument` after capture all `arguments`",
                        "capture all `arguments` is defined here"));
                }
                self.arguments.push(Arg {
                    field: ident,
                    kind: ArgKind::Value { option: is_option },
                    decode: attrs.decode.clone().unwrap_or(DecodeMode::Normal),
                });
            }
            Some(FieldMode::Arguments) => {
                if let Some(prev) = &self.var_args {
                    return Err(err_pair(ident, &prev.field,
                        "only single `arguments` allowed",
                        "previous `arguments` is defined here"));
                }
                self.var_args = Some(VarArgs {
                    field: ident,
                    decode: attrs.decode.clone().unwrap_or(DecodeMode::Normal),
                });
            }
            Some(FieldMode::Property) => {
                if let Some(prev) = &self.var_props {
                    return Err(err_pair(ident, &prev.field,
                        "extra `property` after capture all `properties`",
                        "capture all `properties` is defined here"));
                }
                self.properties.push(Prop {
                    field: ident,
                    option: is_option,
                    decode: attrs.decode.clone().unwrap_or(DecodeMode::Normal),
                    flatten: false,
                });
            }
            Some(FieldMode::Properties) => {
                if let Some(prev) = &self.var_props {
                    return Err(err_pair(ident, &prev.field,
                        "only single `properties` is allowed",
                        "previous `properties` is defined here"));
                }
                self.var_props = Some(VarProps {
                    field: ident,
                    decode: attrs.decode.clone().unwrap_or(DecodeMode::Normal),
                });
            }
            Some(FieldMode::Child) => {
                if let Some(prev) = &self.var_children {
                    return Err(err_pair(ident, &prev.field,
                        "extra `child` after capture all `children`",
                        "capture all `children` is defined here"));
                }
                let name = heck::KebabCase::to_kebab_case(
                    &ident.to_string()[..]);
                self.children.push(Child {
                    name,
                    field: ident,
                    option: is_option,
                    unwrap: attrs.unwrap.as_ref().map(|v| (**v).clone()),
                    flatten: false,
                });
            }
            Some(FieldMode::Children) => {
                if let Some(prev) = &self.var_children {
                    return Err(err_pair(ident, &prev.field,
                        "only single catch all `children` is allowed",
                        "previous `children` is defined here"));
                }
                self.var_children = Some(VarChildren {
                    field: ident,
                });
            }
            Some(FieldMode::Flatten(flatten)) => {
                if is_option {
                    return Err(syn::Error::new_spanned(ident,
                        "optional flatten fields are not supported yet"));
                }
                if flatten.property {
                    if let Some(prev) = &self.var_props {
                        return Err(err_pair(ident, &prev.field,
                            "extra `flatten(property)` after \
                            capture all `properties`",
                            "capture all `properties` is defined here"));
                    }
                    self.properties.push(Prop {
                        field: ident.clone(),
                        option: is_option,
                        decode: DecodeMode::Normal,
                        flatten: true,
                    });
                }
                if flatten.child {
                    if let Some(prev) = &self.var_children {
                        return Err(err_pair(ident, &prev.field,
                            "extra `flatten(child)` after \
                            capture all `children`",
                            "capture all `children` is defined here"));
                    }
                    self.children.push(Child {
                        name: "".into(), // unused
                        field: ident.clone(),
                        option: is_option,
                        unwrap: None,
                        flatten: true,
                    });
                }
            }
            None => {
                self.extra_fields.push(ExtraField {
                    ident,
                    kind: ExtraKind::Default,
                });
            }
        }
        return Ok(self);
    }
}

impl Struct {
    fn new(ident: syn::Ident, generics: syn::Generics,
           _attrs: Vec<syn::Attribute>,
           fields: impl Iterator<Item=syn::Field>)
        -> syn::Result<Self>
    {
        let mut bld = StructBuilder::new(ident, generics);
        for fld in fields {
            let mut attrs = FieldAttrs::new();
            for attr in fld.attrs {
                if matches!(attr.style, syn::AttrStyle::Outer) &&
                    attr.path.is_ident("knuffel")

                {
                    let chunk = attr.parse_args_with(parse_attrs)?;
                    attrs.update(chunk)?;
                }
            }
            bld.add_field(fld.ident.unwrap(), is_option(&fld.ty), &attrs)?;
        }

        Ok(bld.build())
    }
    pub fn all_fields(&self) -> Vec<&syn::Ident> {
        let mut res = Vec::new();
        res.extend(self.arguments.iter().map(|a| &a.field));
        res.extend(self.var_args.iter().map(|a| &a.field));
        res.extend(self.properties.iter().map(|p| &p.field));
        res.extend(self.var_props.iter().map(|p| &p.field));
        res.extend(self.children.iter().map(|c| &c.field));
        res.extend(self.var_children.iter().map(|c| &c.field));
        res.extend(self.extra_fields.iter().map(|f| &f.ident));
        return res;
    }
}

impl Parse for Definition {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut attrs = input.call(syn::Attribute::parse_outer)?;
        let ahead = input.fork();
        let _vis: syn::Visibility = ahead.parse()?;

        let lookahead = ahead.lookahead1();
        if lookahead.peek(syn::Token![struct]) {
            let item: syn::ItemStruct = input.parse()?;
            attrs.extend(item.attrs);
            match item.fields {
                syn::Fields::Named(n) => {
                    Struct::new(item.ident, item.generics, attrs,
                                n.named.into_iter())
                    .map(Definition::Struct)
                }
                syn::Fields::Unnamed(u) => {
                    TupleStruct::new(item.ident, item.generics, attrs,
                                     u.unnamed.into_iter())
                    .map(Definition::TupleStruct)
                }
                syn::Fields::Unit => {
                    UnitStruct::new(item.ident, item.generics, attrs)
                    .map(Definition::UnitStruct)
                }
            }
        } else if lookahead.peek(syn::Token![enum]) {
            let item: syn::ItemEnum = input.parse()?;
            attrs.extend(item.attrs);
            Enum::new(item.ident, item.generics, attrs,
                      item.variants.into_iter())
                .map(Definition::Enum)
        } else {
            Err(lookahead.error())
        }
    }
}

impl FieldAttrs {
    fn new() -> FieldAttrs {
        FieldAttrs {
            mode: None,
            decode: None,
            unwrap: None,
        }
    }
    fn update(&mut self, attrs: impl IntoIterator<Item=(Attr, Span)>)
        -> syn::Result<()>
    {
        use Attr::*;

        for (attr, span) in attrs {
            match attr {
                FieldMode(mode) => {
                    if self.mode.is_some() {
                        return Err(syn::Error::new(span,
                            "only single attribute that defines mode of the \
                            field is allowed. Perhaps you mean `unwrap`?"));
                    }
                    self.mode = Some(mode);
                }
                Unwrap(val) => {
                    if self.unwrap.is_some() {
                        return Err(syn::Error::new(span,
                            "`unwrap` specified twice"));
                    }
                    self.unwrap = Some(Box::new(val));
                }
                DecodeMode(mode) => {
                    if self.decode.is_some() {
                        return Err(syn::Error::new(span,
                            "only single attribute that defines parser of the \
                            field is allowed."));
                    }
                    self.decode = Some(mode);

                }
                _ => return Err(syn::Error::new(span,
                    "this attribute is not supported on fields")),
            }
        }
        Ok(())
    }
}

impl VariantAttrs {
    fn new() -> VariantAttrs {
        VariantAttrs {
            skip: false,
        }
    }
    fn update(&mut self, attrs: impl IntoIterator<Item=(Attr, Span)>)
        -> syn::Result<()>
    {
        use Attr::*;

        for (attr, span) in attrs {
            match attr {
                Skip => self.skip = true,
                _ => return Err(syn::Error::new(span,
                    "this attribute is not supported on enum variants")),
            }
        }
        Ok(())
    }
}

fn parse_attrs(input: ParseStream)
    -> syn::Result<impl IntoIterator<Item=(Attr, Span)>>
{
    Punctuated::<_, syn::Token![,]>::parse_terminated_with(
        input, Attr::parse)
}

impl Attr {
    fn parse(input: ParseStream) -> syn::Result<(Self, Span)> {
        Self::_parse(input).map(|a| (a, input.span()))
    }
    fn _parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(kw::argument) {
            let _kw: kw::argument = input.parse()?;
            Ok(Attr::FieldMode(FieldMode::Argument))
        } else if lookahead.peek(kw::arguments) {
            let _kw: kw::arguments = input.parse()?;
            Ok(Attr::FieldMode(FieldMode::Arguments))
        } else if lookahead.peek(kw::property) {
            let _kw: kw::property = input.parse()?;
            Ok(Attr::FieldMode(FieldMode::Property))
        } else if lookahead.peek(kw::properties) {
            let _kw: kw::properties = input.parse()?;
            Ok(Attr::FieldMode(FieldMode::Properties))
        } else if lookahead.peek(kw::children) {
            let _kw: kw::children = input.parse()?;
            Ok(Attr::FieldMode(FieldMode::Children))
        } else if lookahead.peek(kw::child) {
            let _kw: kw::child = input.parse()?;
            Ok(Attr::FieldMode(FieldMode::Child))
        } else if lookahead.peek(kw::unwrap) {
            let _kw: kw::unwrap = input.parse()?;
            let parens;
            syn::parenthesized!(parens in input);
            let mut attrs = FieldAttrs::new();
            let chunk = parens.call(parse_attrs)?;
            attrs.update(chunk)?;
            Ok(Attr::Unwrap(attrs))
        } else if lookahead.peek(kw::skip) {
            let _kw: kw::skip = input.parse()?;
            Ok(Attr::Skip)
        } else if lookahead.peek(kw::str) {
            let _kw: kw::str = input.parse()?;
            Ok(Attr::DecodeMode(DecodeMode::Str))
        } else if lookahead.peek(kw::flatten) {
            let _kw: kw::flatten = input.parse()?;
            let parens;
            syn::parenthesized!(parens in input);
            let items = Punctuated::<FlattenItem, syn::Token![,]>::
                parse_terminated(&parens)?;
            let mut flatten = Flatten {
                child: false,
                property: false,
            };
            for item in items {
                match item {
                    FlattenItem::Child => flatten.child = true,
                    FlattenItem::Property => flatten.property = true,
                }
            }
            Ok(Attr::FieldMode(FieldMode::Flatten(flatten)))
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for FlattenItem {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(kw::child) {
            let _kw: kw::child = input.parse()?;
            Ok(FlattenItem::Child)
        } else if lookahead.peek(kw::property) {
            let _kw: kw::property = input.parse()?;
            Ok(FlattenItem::Property)
        } else {
            Err(lookahead.error())
        }
    }
}

impl Prop {
    pub fn name(&self) -> String {
        self.field.to_string()
    }
}
