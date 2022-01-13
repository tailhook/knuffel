use std::fmt;

use miette::Diagnostic;

use knuffel::{Decode, span::Span};


#[derive(Debug, Decode, PartialEq)]
struct Unit;

#[derive(Debug, Decode, PartialEq)]
struct Arg(#[knuffel(argument)] u32);

#[derive(Debug, Decode, PartialEq)]
struct Opt(Option<Arg>);

#[derive(Debug, Decode, PartialEq)]
struct Extra(#[knuffel(argument)] Option<String>, u32);

#[derive(Debug, Decode, PartialEq)]
enum Enum {
    Unit,
    Arg(#[knuffel(argument)] u32),
    Opt(Option<Arg>),
    Extra(#[knuffel(argument)] Option<String>, u32),
}


fn parse<T: Decode<Span>>(text: &str) -> T {
    let mut nodes: Vec<T> = knuffel::parse("<test>", text).unwrap();
    assert_eq!(nodes.len(), 1);
    nodes.remove(0)
}

fn parse_err<T: Decode<Span>+fmt::Debug>(text: &str) -> String {
    let err = knuffel::parse::<Vec<T>>("<test>", text).unwrap_err();
    err.related().unwrap()
        .map(|e| e.to_string()).collect::<Vec<_>>()
        .join("\n")
}

#[test]
fn parse_unit() {
    assert_eq!(parse::<Unit>(r#"node"#), Unit);
    assert_eq!(parse_err::<Unit>(r#"node something="world""#),
        "unexpected property `something`");
}

#[test]
fn parse_arg() {
    assert_eq!(parse::<Arg>(r#"node 123"#), Arg(123));
    assert_eq!(parse_err::<Arg>(r#"node something="world""#),
        "additional argument is required");
}

#[test]
fn parse_extra() {
    assert_eq!(parse::<Extra>(r#"node "123""#), Extra(Some("123".into()), 0));
    assert_eq!(parse::<Extra>(r#"node"#), Extra(None, 0));
    assert_eq!(parse_err::<Extra>(r#"node "123" 456"#),
        "unexpected argument");
}

#[test]
fn parse_opt() {
    assert_eq!(parse::<Opt>(r#"node 123"#), Opt(Some(Arg(123))));
    assert_eq!(parse::<Opt>(r#"node"#), Opt(None));
    assert_eq!(parse_err::<Opt>(r#"node something="world""#),
        "additional argument is required");
}

#[test]
fn parse_enum() {
    assert_eq!(parse::<Enum>(r#"unit"#), Enum::Unit);
    assert_eq!(parse::<Enum>(r#"arg 123"#), Enum::Arg(123));
    assert_eq!(parse::<Enum>(r#"opt 123"#), Enum::Opt(Some(Arg(123))));
    assert_eq!(parse::<Enum>(r#"opt"#), Enum::Opt(None));
    assert_eq!(parse::<Enum>(r#"extra"#), Enum::Extra(None, 0));
    assert_eq!(parse_err::<Enum>(r#"unit something="world""#),
        "unexpected property `something`");
    assert_eq!(parse_err::<Enum>(r#"other something="world""#),
        "expected `unit`, `arg`, or one of 2 others");
    assert_eq!(parse_err::<Enum>(r#"extra "hello" "world""#),
        "unexpected argument");
}
