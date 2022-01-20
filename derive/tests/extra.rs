use knuffel::span::Span;
use knuffel::traits::Decode;
use knuffel::ast::{TypeName, BuiltinType};

#[derive(knuffel_derive::Decode, Debug, PartialEq)]
struct Child;

#[derive(knuffel_derive::Decode, Debug, PartialEq)]
#[knuffel(span_type=Span)]
struct NodeSpan {
    #[knuffel(span)]
    span: Span,
    #[knuffel(argument)]
    name: String,
    #[knuffel(children)]
    children: Vec<Child>,
}

#[derive(knuffel_derive::Decode, Debug, PartialEq)]
struct NodeType {
    #[knuffel(type_name)]
    type_name: String,
}

#[derive(knuffel_derive::Decode, Debug, PartialEq)]
struct NameAndType {
    #[knuffel(node_name)]
    node_name: String,
    #[knuffel(type_name)]
    type_name: Option<TypeName>,
}

fn parse<T: Decode<Span>>(text: &str) -> T {
    let mut nodes: Vec<T> = knuffel::parse("<test>", text).unwrap();
    assert_eq!(nodes.len(), 1);
    nodes.remove(0)
}

#[test]
fn parse_node_span() {
    assert_eq!(parse::<NodeSpan>(r#"node "hello""#),
               NodeSpan {
                   span: Span(0, 12),
                   name: "hello".into(),
                   children: Vec::new(),
               });
    assert_eq!(parse::<NodeSpan>(r#"   node  "hello"     "#),
               NodeSpan {
                   span: Span(3, 21),
                   name: "hello".into(),
                   children: Vec::new(),
               });
    assert_eq!(parse::<NodeSpan>(r#"   node  "hello";     "#),
               NodeSpan {
                   span: Span(3, 17),
                   name: "hello".into(),
                   children: Vec::new(),
               });
    assert_eq!(parse::<NodeSpan>(r#"   node  "hello"     {   child;   }"#),
               NodeSpan {
                   span: Span(3, 35),
                   name: "hello".into(),
                   children: vec![Child],
               });
}

#[test]
fn parse_node_type() {
    assert_eq!(parse::<NodeType>(r#"(unknown)node {}"#),
               NodeType { type_name: "unknown".into() });
}

#[test]
fn parse_name_and_type() {
    assert_eq!(parse::<NameAndType>(r#"(u32)nodexxx"#),
               NameAndType {
                   node_name: "nodexxx".into(),
                   type_name: Some(BuiltinType::U32.into()),
               });

    assert_eq!(parse::<NameAndType>(r#"yyynode /-{   }"#),
               NameAndType {
                   node_name: "yyynode".into(),
                   type_name: None,
               });
}
