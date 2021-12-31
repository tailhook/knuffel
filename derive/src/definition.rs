use proc_macro2::{TokenStream, Span};
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;

use crate::kw;

pub enum Definition {
    UnitStruct(Struct),
    TupleStruct(Struct),
    NewType(NewType),
    Struct(Struct),
    Enum(Enum),
}

pub enum VariantKind {
    Unit,
    Nested { option: bool },
    Tuple(Struct),
    Named(Struct),
}

pub enum ArgKind {
    Value { option: bool },
}

#[derive(Debug, Clone)]
pub enum FieldMode {
    Argument,
    Property { name: Option<String> },
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
    Bytes,
}

#[derive(Debug)]
pub enum Attr {
    Skip,
    DecodeMode(DecodeMode),
    FieldMode(FieldMode),
    Unwrap(FieldAttrs),
    Default(Option<syn::Expr>),
}

#[derive(Debug, Clone)]
pub struct FieldAttrs {
    pub mode: Option<FieldMode>,
    pub decode: Option<DecodeMode>,
    pub unwrap: Option<Box<FieldAttrs>>,
    pub default: Option<Option<syn::Expr>>,
}

#[derive(Debug, Clone)]
pub struct VariantAttrs {
    pub skip: bool,
    pub inner: StructAttrs,
}

#[derive(Debug, Clone)]
pub struct StructAttrs {
}

#[derive(Clone)]
pub enum AttrAccess {
    Indexed(usize),
    Named(syn::Ident),
}

#[derive(Clone)]
pub struct Field {
    pub span: Span,
    pub attr: AttrAccess,
    pub tmp_name: syn::Ident,
}

pub struct Arg {
    pub field: Field,
    pub kind: ArgKind,
    pub decode: DecodeMode,
    pub default: Option<Option<syn::Expr>>,
}

pub struct VarArgs {
    pub field: Field,
    pub decode: DecodeMode,
}

pub struct Prop {
    pub field: Field,
    pub name: String,
    pub option: bool,
    pub decode: DecodeMode,
    pub flatten: bool,
    pub default: Option<Option<syn::Expr>>,
}

pub struct VarProps {
    pub field: Field,
    pub decode: DecodeMode,
}

pub struct Child {
    pub field: Field,
    pub name: String,
    pub option: bool,
    pub unwrap: Option<FieldAttrs>,
    pub flatten: bool,
    pub default: Option<Option<syn::Expr>>,
}

pub struct VarChildren {
    pub field: Field,
}

pub enum ExtraKind {
    Auto,
}

pub struct ExtraField {
    pub field: Field,
    pub kind: ExtraKind,
    pub option: bool,
}

pub struct Struct {
    pub ident: syn::Ident,
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
    pub arguments: Vec<Arg>,
    pub var_args: Option<VarArgs>,
    pub properties: Vec<Prop>,
    pub var_props: Option<VarProps>,
    pub children: Vec<Child>,
    pub var_children: Option<VarChildren>,
    pub extra_fields: Vec<ExtraField>,
}

pub struct NewType {
    pub ident: syn::Ident,
    pub option: bool,
}

pub struct Variant {
    pub ident: syn::Ident,
    pub name: String,
    pub kind: VariantKind,
}

pub struct Enum {
    pub ident: syn::Ident,
    pub variants: Vec<Variant>,
}


fn err_pair(s1: &Field, s2: &Field, t1: &str, t2: &str)
    -> syn::Error
{
    let mut err = syn::Error::new(s1.span, t1);
    err.combine(syn::Error::new(s2.span, t2));
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
    fn new(ident: syn::Ident, _attrs: VariantAttrs, kind: VariantKind)
        -> syn::Result<Self>
    {
        let name = heck::KebabCase::to_kebab_case(&ident.to_string()[..]);
        Ok(Variant {
            ident,
            name,
            kind,
        })
    }
}

impl Enum {
    fn new(ident: syn::Ident, _attrs: Vec<syn::Attribute>,
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
            let kind = match var.fields {
                syn::Fields::Named(n) => {
                    Struct::new(var.ident.clone(), &attrs.inner,
                                n.named.into_iter())
                    .map(VariantKind::Named)?
                }
                syn::Fields::Unnamed(u) => {
                    let tup = Struct::new(var.ident.clone(), &attrs.inner,
                                          u.unnamed.into_iter())?;
                    if tup.all_fields().len() == 1
                        && tup.extra_fields.len() == 1
                        && matches!(tup.extra_fields[0].kind, ExtraKind::Auto)
                    {
                        // Single tuple variant without any defition means
                        // the first field inside is meant to be full node
                        // parser.
                        VariantKind::Nested {
                            option: tup.extra_fields[0].option,
                        }
                    } else {
                        VariantKind::Tuple(tup)
                    }
                }
                syn::Fields::Unit => {
                    VariantKind::Unit
                }
            };
            variants.push(Variant::new(var.ident, attrs, kind)?);
        }
        Ok(Enum {
            ident,
            variants,
        })
    }
}

impl StructBuilder {
    pub fn new(ident: syn::Ident) -> Self {
        StructBuilder {
            ident,
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
    pub fn add_field(&mut self, field: Field, is_option: bool,
                     attrs: &FieldAttrs)
        -> syn::Result<&mut Self>
    {
        match &attrs.mode {
            Some(FieldMode::Argument) => {
                if let Some(prev) = &self.var_args {
                    return Err(err_pair(&field, &prev.field,
                        "extra `argument` after capture all `arguments`",
                        "capture all `arguments` is defined here"));
                }
                self.arguments.push(Arg {
                    field,
                    kind: ArgKind::Value { option: is_option },
                    decode: attrs.decode.clone().unwrap_or(DecodeMode::Normal),
                    default: attrs.default.clone(),
                });
            }
            Some(FieldMode::Arguments) => {
                if let Some(prev) = &self.var_args {
                    return Err(err_pair(&field, &prev.field,
                        "only single `arguments` allowed",
                        "previous `arguments` is defined here"));
                }
                self.var_args = Some(VarArgs {
                    field,
                    decode: attrs.decode.clone().unwrap_or(DecodeMode::Normal),
                });
            }
            Some(FieldMode::Property { name }) => {
                if let Some(prev) = &self.var_props {
                    return Err(err_pair(&field, &prev.field,
                        "extra `property` after capture all `properties`",
                        "capture all `properties` is defined here"));
                }
                let name = match (name, &field.attr) {
                    (Some(name), _) => name.clone(),
                    (None, AttrAccess::Named(name)) => name.to_string(),
                    (None, AttrAccess::Indexed(_)) => {
                        return Err(syn::Error::new(field.span,
                            "property must be named, try \
                             `property(name=\"something\")"));
                    }
                };
                self.properties.push(Prop {
                    field,
                    name,
                    option: is_option,
                    decode: attrs.decode.clone().unwrap_or(DecodeMode::Normal),
                    flatten: false,
                    default: attrs.default.clone(),
                });
            }
            Some(FieldMode::Properties) => {
                if let Some(prev) = &self.var_props {
                    return Err(err_pair(&field, &prev.field,
                        "only single `properties` is allowed",
                        "previous `properties` is defined here"));
                }
                self.var_props = Some(VarProps {
                    field,
                    decode: attrs.decode.clone().unwrap_or(DecodeMode::Normal),
                });
            }
            Some(FieldMode::Child) => {
                if let Some(prev) = &self.var_children {
                    return Err(err_pair(&field, &prev.field,
                        "extra `child` after capture all `children`",
                        "capture all `children` is defined here"));
                }
                let name = match &field.attr {
                    AttrAccess::Named(n) => {
                        heck::KebabCase::to_kebab_case(&n.to_string()[..])
                    }
                    AttrAccess::Indexed(_) => {
                        return Err(syn::Error::new(field.span,
                            "`child` is not allowed for tuple structs"));
                    }
                };
                self.children.push(Child {
                    name,
                    field,
                    option: is_option,
                    unwrap: attrs.unwrap.as_ref().map(|v| (**v).clone()),
                    flatten: false,
                    default: attrs.default.clone(),
                });
            }
            Some(FieldMode::Children) => {
                if let Some(prev) = &self.var_children {
                    return Err(err_pair(&field, &prev.field,
                        "only single catch all `children` is allowed",
                        "previous `children` is defined here"));
                }
                self.var_children = Some(VarChildren {
                    field,
                });
            }
            Some(FieldMode::Flatten(flatten)) => {
                if is_option {
                    return Err(syn::Error::new(field.span,
                        "optional flatten fields are not supported yet"));
                }
                if flatten.property {
                    if let Some(prev) = &self.var_props {
                        return Err(err_pair(&field, &prev.field,
                            "extra `flatten(property)` after \
                            capture all `properties`",
                            "capture all `properties` is defined here"));
                    }
                    self.properties.push(Prop {
                        field: field.clone(),
                        name: "".into(),  // irrelevant
                        option: is_option,
                        decode: DecodeMode::Normal,
                        flatten: true,
                        default: None,
                    });
                }
                if flatten.child {
                    if let Some(prev) = &self.var_children {
                        return Err(err_pair(&field, &prev.field,
                            "extra `flatten(child)` after \
                            capture all `children`",
                            "capture all `children` is defined here"));
                    }
                    self.children.push(Child {
                        name: "".into(), // unused
                        field: field.clone(),
                        option: is_option,
                        unwrap: None,
                        flatten: true,
                        default: None,
                    });
                }
            }
            None => {
                self.extra_fields.push(ExtraField {
                    field,
                    kind: ExtraKind::Auto,
                    option: is_option,
                });
            }
        }
        return Ok(self);
    }
}

impl Struct {
    fn new(ident: syn::Ident, _attrs: &StructAttrs,
           fields: impl Iterator<Item=syn::Field>)
        -> syn::Result<Self>
    {
        let mut bld = StructBuilder::new(ident);
        for (idx, fld) in fields.enumerate() {
            let mut attrs = FieldAttrs::new();
            for attr in &fld.attrs {
                if matches!(attr.style, syn::AttrStyle::Outer) &&
                    attr.path.is_ident("knuffel")

                {
                    let chunk = attr.parse_args_with(parse_attrs)?;
                    attrs.update(chunk)?;
                }
            }
            let field = Field::new(&fld, idx);
            bld.add_field(field, is_option(&fld.ty), &attrs)?;
        }

        Ok(bld.build())
    }
    pub fn all_fields(&self) -> Vec<&Field> {
        let mut res = Vec::new();
        res.extend(self.arguments.iter().map(|a| &a.field));
        res.extend(self.var_args.iter().map(|a| &a.field));
        res.extend(self.properties.iter().map(|p| &p.field));
        res.extend(self.var_props.iter().map(|p| &p.field));
        res.extend(self.children.iter().map(|c| &c.field));
        res.extend(self.var_children.iter().map(|c| &c.field));
        res.extend(self.extra_fields.iter().map(|f| &f.field));
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
            // TODO(tailhook) parse struct attrs
            let attrs = StructAttrs {};
            match item.fields {
                syn::Fields::Named(n) => {
                    Struct::new(item.ident, &attrs, n.named.into_iter())
                    .map(Definition::Struct)
                }
                syn::Fields::Unnamed(u) => {
                    let tup = Struct::new(item.ident.clone(), &attrs,
                                          u.unnamed.into_iter())?;
                    if tup.all_fields().len() == 1
                        && tup.extra_fields.len() == 1
                        && matches!(tup.extra_fields[0].kind, ExtraKind::Auto)
                    {
                        Ok(Definition::NewType(NewType {
                            ident: item.ident,
                            option: tup.extra_fields[0].option,
                        }))
                    } else {
                        Ok(Definition::TupleStruct(tup))
                    }
                }
                syn::Fields::Unit => {
                    Struct::new(item.ident, &attrs, Vec::new().into_iter())
                    .map(Definition::UnitStruct)
                }
            }
        } else if lookahead.peek(syn::Token![enum]) {
            let item: syn::ItemEnum = input.parse()?;
            attrs.extend(item.attrs);
            Enum::new(item.ident, attrs,
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
            default: None,
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
                            field is allowed"));
                    }
                    self.decode = Some(mode);

                }
                Default(value) => {
                    if self.default.is_some() {
                        return Err(syn::Error::new(span,
                            "only single default is allowed"));
                    }
                    self.default = Some(value);
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
            inner: StructAttrs {},
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
            let mut name = None;
            if !input.is_empty() && !input.lookahead1().peek(syn::Token![,]) {
                let parens;
                syn::parenthesized!(parens in input);
                let lookahead = parens.lookahead1();
                if lookahead.peek(kw::name) {
                    let _kw: kw::name = parens.parse()?;
                    let _eq: syn::Token![=] = parens.parse()?;
                    let name_lit: syn::LitStr = parens.parse()?;
                    name = Some(name_lit.value());
                } else {
                    return Err(lookahead.error())
                }
            }
            Ok(Attr::FieldMode(FieldMode::Property { name }))
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
        } else if lookahead.peek(kw::bytes) {
            let _kw: kw::bytes = input.parse()?;
            Ok(Attr::DecodeMode(DecodeMode::Bytes))
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
        } else if lookahead.peek(kw::default) {
            let _kw: kw::default = input.parse()?;
            if !input.is_empty() && !input.lookahead1().peek(syn::Token![,]) {
                let _eq: syn::Token![=] = input.parse()?;
                let value: syn::Expr = input.parse()?;
                Ok(Attr::Default(Some(value)))
            } else {
                Ok(Attr::Default(None))
            }
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

impl Field {
    pub fn new_named(name: &syn::Ident) -> Field {
        Field {
            span: name.span(),
            attr: AttrAccess::Named(name.clone()),
            tmp_name: name.clone(),
        }
    }
    fn new(field: &syn::Field, idx: usize) -> Field {
        field.ident.as_ref()
            .map(|id| Field {
                span: field.span(),
                attr: AttrAccess::Named(id.clone()),
                tmp_name: id.clone(),
            })
            .unwrap_or_else(|| Field {
                span: field.span(),
                attr: AttrAccess::Indexed(idx),
                tmp_name: syn::Ident::new(
                    &format!("field{}", idx),
                    Span::mixed_site(),
                ),
            })
    }
    pub fn from_self(&self) -> TokenStream {
        match &self.attr {
            AttrAccess::Indexed(idx) => quote!(self.#idx),
            AttrAccess::Named(name) => quote!(self.#name),
        }
    }
    pub fn is_indexed(&self) -> bool {
        matches!(self.attr, AttrAccess::Indexed(_))
    }
    pub fn as_index(&self) -> Option<usize> {
        match &self.attr {
            AttrAccess::Indexed(idx) => Some(*idx),
            AttrAccess::Named(_) => None,
        }
    }
    pub fn as_assign_pair(&self) -> Option<TokenStream> {
        match &self.attr {
            AttrAccess::Indexed(_) => None,
            AttrAccess::Named(n) if n == &self.tmp_name => Some(quote!(#n)),
            AttrAccess::Named(n) => {
                let tmp_name = &self.tmp_name;
                Some(quote!(#n: #tmp_name))
            }
        }
    }
}
