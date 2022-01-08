use std::io::Read;

use knuffel::DecodeChildren;
use miette::IntoDiagnostic;


#[derive(knuffel::Decode, Debug)]
struct Plugin {
    #[knuffel(argument)]
    name: String,
    #[knuffel(property)]
    url: String,
    #[knuffel(child, unwrap(argument))]
    version: String,
}

#[derive(knuffel::Decode, Debug)]
struct Config {
    #[knuffel(child, unwrap(argument))]
    version: String,
    #[knuffel(children(name="plugin"))]
    plugins: Vec<Plugin>,
}

fn main() -> miette::Result<()> {
    miette::set_hook(Box::new(|_| {
        Box::new(miette::MietteHandlerOpts::new()
            .terminal_links(true)
            .unicode(true)
            .force_graphical(true)
            .context_lines(1)
            .tab_width(4)
            .build())
    }))?;
    let mut buf = String::new();
    std::io::stdin().read_to_string(&mut buf).into_diagnostic()?;
    let ast = knuffel::raw_parse(&buf)?;
    println!("{:#?}", ast);
    println!("{:#?}", Config::decode_children(&ast.nodes).into_diagnostic()?);
    Ok(())
}
