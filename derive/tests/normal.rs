use std::fmt;
use std::collections::BTreeMap;

use knuffel::{Decode, span::Span, raw_parse};


#[derive(knuffel_derive::Decode, Debug, PartialEq)]
struct Arg1 {
    #[knuffel(argument)]
    name: String,
}

#[derive(knuffel_derive::Decode, Debug, PartialEq)]
struct OptArg {
    #[knuffel(argument)]
    name: Option<String>,
}

#[derive(knuffel_derive::Decode, Debug, PartialEq)]
struct VarArg {
    #[knuffel(arguments)]
    params: Vec<u64>,
}

#[derive(knuffel_derive::Decode, Debug, PartialEq)]
struct Prop1 {
    #[knuffel(property)]
    label: String,
}

#[derive(knuffel_derive::Decode, Debug, PartialEq)]
struct OptProp {
    #[knuffel(property)]
    label: Option<String>,
}

#[derive(knuffel_derive::Decode, Debug, PartialEq)]
struct VarProp {
    #[knuffel(properties)]
    scores: BTreeMap<String, u64>,
}

#[derive(knuffel_derive::Decode, Debug, PartialEq)]
struct Children {
    #[knuffel(children)]
    children: Vec<Arg1>,
}

#[derive(knuffel_derive::Decode, Debug, PartialEq)]
enum Variant {
    Arg1(Arg1),
    Prop1(Prop1),
}

fn parse<T: Decode<Span>>(text: &str) -> T {
    let doc = raw_parse(text).unwrap();
    T::decode_node(&doc.nodes[0]).unwrap()
}

fn parse_err<T: Decode<Span>+fmt::Debug>(text: &str) -> String {
    let doc = raw_parse(text).unwrap();
    T::decode_node(&doc.nodes[0]).unwrap_err().to_string()
}

#[test]
fn parse_arg1() {
    assert_eq!(parse::<Arg1>(r#"node "hello""#),
               Arg1 { name: "hello".into() } );
    assert_eq!(parse_err::<Arg1>(r#"node "hello" "world""#),
        "13..20: unexpected argument");
    assert_eq!(parse_err::<Arg1>(r#"node"#),
        "0..4: additional argument `name` is required");
}

#[test]
fn parse_opt_arg() {
    assert_eq!(parse::<OptArg>(r#"node "hello""#),
               OptArg { name: Some("hello".into()) } );
    assert_eq!(parse::<OptArg>(r#"node"#),
               OptArg { name: None });
}

#[test]
fn parse_prop() {
    assert_eq!(parse::<Prop1>(r#"node label="hello""#),
               Prop1 { label: "hello".into() } );
    assert_eq!(parse_err::<Prop1>(r#"node label="hello" y="world""#),
        "19..20: unexpected property `y`");
    assert_eq!(parse_err::<Prop1>(r#"node"#),
        "0..4: property `label` is required");
}

#[test]
fn parse_opt_prop() {
    assert_eq!(parse::<OptProp>(r#"node label="hello""#),
               OptProp { label: Some("hello".into()) } );
    assert_eq!(parse::<OptProp>(r#"node"#),
               OptProp { label: None } );
}

#[test]
fn parse_var_arg() {
    assert_eq!(parse::<VarArg>(r#"sum 1 2 3"#),
               VarArg { params: vec![1, 2, 3] } );
    assert_eq!(parse::<VarArg>(r#"sum"#),
               VarArg { params: vec![] } );
}

#[test]
fn parse_var_prop() {
    let mut scores = BTreeMap::new();
    scores.insert("john".into(), 13);
    scores.insert("jack".into(), 7);
    assert_eq!(parse::<VarProp>(r#"scores john=13 jack=7"#),
               VarProp { scores } );
    assert_eq!(parse::<VarProp>(r#"scores"#),
               VarProp { scores: BTreeMap::new() } );
}

#[test]
fn parse_children() {
    assert_eq!(parse::<Children>(r#"parent { - "val1"; - "val2"; }"#),
               Children { children: vec! [
                   Arg1 { name: "val1".into() },
                   Arg1 { name: "val2".into() },
               ]} );
    assert_eq!(parse::<Children>(r#"parent"#),
               Children { children: Vec::new() } );
}

#[test]
fn parse_enum() {
    assert_eq!(parse::<Variant>(r#"arg1 "hello""#),
               Variant::Arg1(Arg1 { name: "hello".into() }));
    assert_eq!(parse::<Variant>(r#"prop1 label="hello""#),
               Variant::Prop1(Prop1 { label: "hello".into() }));
    assert_eq!(parse_err::<Variant>(r#"something"#),
        "0..9: expected one of `arg1`, `prop1`");
}
