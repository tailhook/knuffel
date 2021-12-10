use std::path::PathBuf;

use knuffel::{Decode, span::Span, raw_parse};


#[derive(knuffel_derive::Decode, Debug, PartialEq)]
struct Scalars {
    #[knuffel(child, unwrap(argument))]
    str: String,
    #[knuffel(child, unwrap(argument))]
    u64: u64,
    #[knuffel(child, unwrap(argument))]
    path: PathBuf,
    #[knuffel(child, unwrap(argument))]
    boolean: bool,
}

fn parse<T: Decode<Span>>(text: &str) -> T {
    let doc = raw_parse(text).unwrap();
    T::decode_children(&doc.nodes).unwrap()
}


#[test]
fn parse_enum() {
    assert_eq!(
        parse::<Scalars>(r#"
            str "hello"
            u64 1234
            path "/hello/world"
            boolean true
        "#),
        Scalars {
            str: "hello".into(),
            u64: 1234,
            path: PathBuf::from("/hello/world"),
            boolean: true,
        });
}
