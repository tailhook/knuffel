use std::fmt;

use knuffel::{Decode};
use knuffel::span::Span;
use miette::Diagnostic;


#[derive(knuffel::DecodeScalar, Debug, PartialEq)]
enum SomeScalar {
    First,
    AnotherOption,
}

#[derive(knuffel::Decode, Debug, PartialEq)]
struct Item {
    #[knuffel(argument)]
    value: SomeScalar,
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
fn parse_some_scalar() {
    assert_eq!(parse::<Item>(r#"node "first""#),
               Item { value: SomeScalar::First } );
    assert_eq!(parse::<Item>(r#"node "another-option""#),
               Item { value: SomeScalar::AnotherOption } );
    assert_eq!(parse_err::<Item>(r#"node "test""#),
        "expected one of `first`, `another-option`");
}
