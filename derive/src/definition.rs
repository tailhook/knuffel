use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;

use crate::kw;

pub enum Definition {
    UnitStruct(UnitStruct),
    TupleStruct(TupleStruct),
    Struct(Struct),
    Enum(syn::ItemEnum),
}

pub enum ArgKind {
    Value { optional: bool },
}

#[derive(Debug)]
pub enum FieldMode {
    Argument,
    Property,
}

#[derive(Debug)]
pub enum Attr {
    FieldMode(FieldMode),
}

#[derive(Debug)]
struct FieldAttrs {
    mode: Option<FieldMode>,
}

pub enum Kind {
    Int,
    Decimal,
    String,
    Bool,
}

pub struct Arg {
    pub field: syn::Ident,
    pub kind: ArgKind,
}

pub struct Prop {
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
    pub properties: Vec<Prop>,
    pub extra_fields: Vec<ExtraField>,
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

impl Struct {
    fn new(ident: syn::Ident, generics: syn::Generics,
           _attrs: Vec<syn::Attribute>,
           fields: impl Iterator<Item=syn::Field>)
        -> syn::Result<Self>
    {
        let mut arguments = Vec::new();
        let mut properties = Vec::new();
        let mut extra_fields = Vec::new();
        for fld in fields {
            let mut attrs = FieldAttrs::new();
            for attr in fld.attrs {
                if matches!(attr.style, syn::AttrStyle::Outer) &&
                    attr.path.is_ident("knuffel")

                {
                    let chunk = attr.parse_args_with(parse_field_attrs)?;
                    attrs.update(chunk);
                }
            }
            match attrs.mode {
                Some(FieldMode::Argument) => {
                    arguments.push(Arg {
                        field: fld.ident.unwrap(),
                        kind: ArgKind::Value { optional: false },
                    });
                }
                Some(FieldMode::Property) => {
                    properties.push(Prop {
                        field: fld.ident.unwrap(),
                    });
                }
                None => {
                    extra_fields.push(ExtraField {
                        ident: fld.ident.unwrap(),
                        kind: ExtraKind::Default,
                    });
                }
            }
        }

        Ok(Struct {
            ident,
            generics,
            arguments,
            properties,
            extra_fields,
        })
    }
    pub fn all_fields(&self) -> Vec<&syn::Ident> {
        let mut res = Vec::new();
        res.extend(self.arguments.iter().map(|a| &a.field));
        res.extend(self.properties.iter().map(|p| &p.field));
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
            input.parse().map(Definition::Enum)
        } else {
            Err(lookahead.error())
        }
    }
}

impl FieldAttrs {
    fn new() -> FieldAttrs {
        FieldAttrs {
            mode: None,
        }
    }
    fn update(&mut self, attrs: impl IntoIterator<Item=Attr>) {
        use Attr::*;

        for attr in attrs {
            match attr {
                FieldMode(mode) => self.mode = Some(mode),
            }
        }
    }
}

fn parse_field_attrs(input: ParseStream)
    -> syn::Result<impl IntoIterator<Item=Attr>>
{
    Punctuated::<_, syn::Token![,]>::parse_terminated_with(
        input, Attr::parse_field)
}

impl Attr {
    fn parse_field(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(kw::argument) {
            let _kw: kw::argument = input.parse()?;
            Ok(Attr::FieldMode(FieldMode::Argument))
        } else if lookahead.peek(kw::property) {
            let _kw: kw::property = input.parse()?;
            Ok(Attr::FieldMode(FieldMode::Property))
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
