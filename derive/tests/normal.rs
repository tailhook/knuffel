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
    #[knuffel(skip)]
    #[allow(dead_code)]
    Var3(u32),
}

#[derive(knuffel_derive::Decode, Debug, PartialEq)]
struct Child {
    #[knuffel(child)]
    main: Prop1,
    #[knuffel(child)]
    extra: Option<Prop1>,
}

#[derive(knuffel_derive::Decode, Debug, PartialEq)]
struct Unwrap {
    #[knuffel(child, unwrap(argument))]
    label: String,
}

fn parse<T: Decode<Span>>(text: &str) -> T {
    let doc = raw_parse(text).unwrap();
    T::decode_node(&doc.nodes[0]).unwrap()
}

fn parse_doc<T: Decode<Span>>(text: &str) -> T {
    let doc = raw_parse(text).unwrap();
    T::decode_children(&doc.nodes).unwrap()
}

fn parse_err<T: Decode<Span>+fmt::Debug>(text: &str) -> String {
    let doc = raw_parse(text).unwrap();
    T::decode_node(&doc.nodes[0]).unwrap_err().to_string()
}

fn parse_doc_err<T: Decode<Span>+fmt::Debug>(text: &str) -> String {
    let doc = raw_parse(text).unwrap();
    T::decode_children(&doc.nodes).unwrap_err().to_string()
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
fn parse_unwrap() {
    assert_eq!(parse::<Unwrap>(r#"node { label "hello"; }"#),
               Unwrap { label: "hello".into() } );
    assert_eq!(parse_err::<Unwrap>(r#"node label="hello""#),
        "5..10: unexpected property `label`");
    assert_eq!(parse_err::<Unwrap>(r#"node"#),
        "0..4: child node `label` is required");
    assert_eq!(parse_doc::<Unwrap>(r#"label "hello""#),
               Unwrap { label: "hello".into() } );
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

    assert_eq!(parse_doc::<Children>(r#"- "val1"; - "val2""#),
               Children { children: vec! [
                   Arg1 { name: "val1".into() },
                   Arg1 { name: "val2".into() },
               ]} );
    assert_eq!(parse_doc::<Children>(r#""#),
               Children { children: Vec::new() } );
}

#[test]
fn parse_child() {
    assert_eq!(parse::<Child>(r#"parent { main label="val1"; }"#),
               Child {
                   main: Prop1 { label: "val1".into() },
                   extra: None,
               });
    assert_eq!(parse::<Child>(r#"parent {
                    main label="primary";
                    extra label="replica";
                 }"#),
               Child {
                   main: Prop1 { label: "primary".into() },
                   extra: Some(Prop1 { label: "replica".into() }),
               });
    assert_eq!(parse_err::<Child>(r#"parent { something; }"#),
               "9..19: unexpected node `something`");
    assert_eq!(parse_err::<Child>(r#"parent"#),
               "0..6: child node `main` is required");

    assert_eq!(parse_doc::<Child>(r#"main label="val1""#),
               Child {
                   main: Prop1 { label: "val1".into() },
                   extra: None,
               });
    assert_eq!(parse_doc::<Child>(r#"
                    main label="primary"
                    extra label="replica"
                 "#),
               Child {
                   main: Prop1 { label: "primary".into() },
                   extra: Some(Prop1 { label: "replica".into() }),
               });
    assert_eq!(parse_doc_err::<Child>(r#"something"#),
               "0..9: unexpected node `something`");
    assert_eq!(parse_doc_err::<Child>(r#""#),
               "child node `main` is required");
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
