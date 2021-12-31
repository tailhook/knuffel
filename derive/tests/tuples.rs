use std::fmt;

use knuffel::{Decode, span::Span, raw_parse};


#[derive(Debug, Decode, PartialEq)]
struct Unit;

#[derive(Debug, Decode, PartialEq)]
struct Arg(#[knuffel(argument)] u32);

#[derive(Debug, Decode, PartialEq)]
struct Opt(Option<Arg>);

#[derive(Debug, Decode, PartialEq)]
enum Enum {
    Unit,
    Arg(#[knuffel(argument)] u32),
    Opt(Option<Arg>),
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
fn parse_unit() {
    assert_eq!(parse::<Unit>(r#"node"#), Unit);
    assert_eq!(parse_err::<Unit>(r#"node something="world""#),
        "5..14: unexpected property `something`");
}

#[test]
fn parse_arg() {
    assert_eq!(parse::<Arg>(r#"node 123"#), Arg(123));
    assert_eq!(parse_err::<Arg>(r#"node something="world""#),
        "0..4: additional argument is required");
}

#[test]
fn parse_opt() {
    assert_eq!(parse::<Opt>(r#"node 123"#), Opt(Some(Arg(123))));
    assert_eq!(parse::<Opt>(r#"node"#), Opt(None));
    assert_eq!(parse_err::<Opt>(r#"node something="world""#),
        "0..4: additional argument is required");
}

#[test]
fn parse_enum() {
    assert_eq!(parse::<Enum>(r#"unit"#), Enum::Unit);
    assert_eq!(parse::<Enum>(r#"arg 123"#), Enum::Arg(123));
    assert_eq!(parse::<Enum>(r#"opt 123"#), Enum::Opt(Some(Arg(123))));
    assert_eq!(parse::<Enum>(r#"opt"#), Enum::Opt(None));
    assert_eq!(parse_err::<Enum>(r#"unit something="world""#),
        "5..14: unexpected property `something`");
    assert_eq!(parse_err::<Enum>(r#"other something="world""#),
        "0..5: expected one of `unit`, `arg`, `opt`");
}
