use std::fmt;

use knuffel::{Decode, span::Span, raw_parse};
use knuffel::traits::DecodeChildren;


#[derive(knuffel_derive::Decode, Default, Debug, PartialEq)]
struct Prop1 {
    #[knuffel(property)]
    label: Option<String>,
}

#[derive(knuffel_derive::Decode, Debug, PartialEq)]
struct FlatProp {
    #[knuffel(flatten(property))]
    props: Prop1,
}

#[derive(knuffel_derive::Decode, Default, Debug, PartialEq)]
struct Unwrap {
    #[knuffel(child, unwrap(argument))]
    label: Option<String>,
}

#[derive(knuffel_derive::Decode, Debug, PartialEq)]
struct FlatChild {
    #[knuffel(flatten(child))]
    children: Unwrap,
}


fn parse<T: Decode<Span>>(text: &str) -> T {
    let doc = raw_parse(text).unwrap();
    T::decode_node(&doc.nodes[0]).unwrap()
}

fn parse_doc<T: DecodeChildren<Span>>(text: &str) -> T {
    let doc = raw_parse(text).unwrap();
    T::decode_children(&doc.nodes).unwrap()
}

fn parse_err<T: Decode<Span>+fmt::Debug>(text: &str) -> String {
    let doc = raw_parse(text).unwrap();
    T::decode_node(&doc.nodes[0]).unwrap_err().to_string()
}

fn parse_doc_err<T: DecodeChildren<Span>+fmt::Debug>(text: &str) -> String {
    let doc = raw_parse(text).unwrap();
    T::decode_children(&doc.nodes).unwrap_err().to_string()
}

#[test]
fn parse_flat_prop() {
    assert_eq!(parse::<FlatProp>(r#"node label="hello""#),
        FlatProp { props: Prop1 { label: Some("hello".into()) } } );
    assert_eq!(parse_err::<FlatProp>(r#"node something="world""#),
        "5..14: unexpected property `something`");
}

#[test]
fn parse_flat_child() {
    assert_eq!(parse_doc::<FlatChild>(r#"label "hello""#),
        FlatChild { children: Unwrap { label: Some("hello".into()) } } );
    assert_eq!(parse_doc_err::<FlatChild>(r#"something "world""#),
        "0..17: unexpected node `something`");
}
