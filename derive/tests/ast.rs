use knuffel::span::Span;
use knuffel::traits::Decode;

#[derive(knuffel_derive::Decode, Debug)]
#[knuffel(span_type=knuffel::span::Span)]
struct AstChildren {
    #[knuffel(children)]
    children: Vec<knuffel::ast::SpannedNode<Span>>,
}

fn parse<T: Decode<Span>>(text: &str) -> T {
    let mut nodes: Vec<T> = knuffel::parse("<test>", text).unwrap();
    assert_eq!(nodes.len(), 1);
    nodes.remove(0)
}

#[test]
fn parse_node_span() {
    let item = parse::<AstChildren>(r#"node {a; b;}"#);
    assert_eq!(item.children.len(), 2);
}
