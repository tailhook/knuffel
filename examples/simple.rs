use std::io::Read;

use miette::IntoDiagnostic;


#[derive(knuffel::Decode, Debug)]
#[allow(dead_code)]
struct Plugin {
    #[knuffel(argument)]
    name: String,
    #[knuffel(property)]
    url: String,
    #[knuffel(child, unwrap(argument))]
    version: String,
}

#[derive(knuffel::Decode, Debug)]
#[allow(dead_code)]
struct Config {
    #[knuffel(child, unwrap(argument))]
    version: String,
    #[knuffel(children(name="plugin"))]
    plugins: Vec<Plugin>,
}

fn main() -> miette::Result<()> {
    let mut buf = String::new();
    println!("Please type KDL document, press Return, Ctrl+D to finish");
    std::io::stdin().read_to_string(&mut buf).into_diagnostic()?;
    let cfg: Config = knuffel::parse("<stdin>", &buf)?;
    println!("{:#?}", cfg);
    Ok(())
}
