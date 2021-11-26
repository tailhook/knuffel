use std::fmt;

use knuffel::{Decode, span::Span, raw_parse};


#[derive(knuffel_derive::Decode, Debug, PartialEq)]
struct Arg1 {
    #[knuffel(argument)]
    name: String,
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
