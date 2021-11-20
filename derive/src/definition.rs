pub enum Definition {
    Struct(Struct),
    Enum(syn::ItemEnum),
}

pub enum ArgKind {
    Const(syn::Expr),
    Value { kind: Kind, optional: bool },
}

pub enum Kind {
    Int,
    Decimal,
    String,
    Bool,
}

pub struct Arg {
    pub ident: syn::Ident,
    pub default: syn::Expr,
    pub kind: ArgKind,
}

pub enum ExtraKind {
    Default,
    Value(syn::Expr),
}

pub struct ExtraField {
    pub ident: syn::Ident,
    pub kind: ExtraKind,
}

pub struct Struct {
    pub ident: syn::Ident,
    pub generics: syn::Generics,
    pub arguments: Vec<Arg>,
    pub extra_fields: Vec<ExtraField>,
}

impl TryFrom<syn::ItemStruct> for Struct {
    type Error = syn::Error;
    fn try_from(s: syn::ItemStruct) -> syn::Result<Struct> {
        let mut arguments = Vec::new();
        let mut extra_fields = Vec::new();

        Ok(Struct {
            ident: s.ident,
            generics: s.generics,
            arguments,
            extra_fields,
        })
    }
}

impl syn::parse::Parse for Definition {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut attrs = input.call(syn::Attribute::parse_outer)?;
        let ahead = input.fork();
        let _vis: syn::Visibility = ahead.parse()?;

        let lookahead = ahead.lookahead1();
        if lookahead.peek(syn::Token![struct]) {
            let mut item: syn::ItemStruct = input.parse()?;
            attrs.extend(item.attrs);
            item.attrs = attrs;
            item.try_into().map(Definition::Struct)
        } else if lookahead.peek(syn::Token![enum]) {
            input.parse().map(Definition::Enum)
        } else {
            Err(lookahead.error())
        }
    }
}
