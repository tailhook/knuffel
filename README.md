A [KDL](https://kdl.dev) configuration file format parser with great error
reporting.

# About KDL

To give you some background on the KDL format. Here is a small example:
```kdl
foo 1 key="val" "three" {
    bar
    (role)baz 1 2
}
```

Here is what are annotations for all the datum as described by the
[specification] and this guide:
```text
foo 1 "three" key="val" {                           ╮
─┬─ ┬ ───┬─── ────┬────                             │
 │  │    │        ╰───── property (can be multiple) │
 │  │    │                                          │
 │  ╰────┴────────────── arguments                  │
 │                                                  │
 └── node name                                      ├─ node "foo", with
                                                    │  "bar" and "baz" being
    bar                                             │  children
    (role)baz 1 2                                   │
     ──┬─                                           │
       └────── type name for node named "baz"       │
}                                                   ╯
```
(note, the order of properties doesn't matter as well as the order of
properties with respect to arguments, so I've moved arguments to have less
intersections for the arrows)

# Usage

Most common usage of this library is using `derive` and [parse] function:
```rust
#[derive(knuffel::Decode)]
enum TopLevelNode {
    Route(Route),
    Plugin(Plugin),
}

#[derive(knuffel::Decode)]
struct Route {
    #[knuffel(argument)]
    path: String,
    #[knuffel(children(name="route"))]
    subroutes: Vec<Route>,
}

#[derive(knuffel::Decode)]
struct Plugin {
    #[knuffel(argument)]
    name: String,
    #[knuffel(property)]
    url: String,
}

# fn main() -> miette::Result<()> {
let config = knuffel::parse::<Vec<TopLevelNode>>("example.kdl", r#"
    route "/api" {
        route "/api/v1"
    }
    plugin "http" url="https://example.org/http"
"#)?;
# Ok(())
# }
```

This parses into a vector of nodes as enums `TopLevelNode`, but you also use some node as a root of the document if there is no properties and arguments declared:
```rust,ignore
#[derive(knuffel::Decode)]
struct Document {
    #[knuffel(child, unwrap(argument))]
    version: Option<String>,
    #[knuffel(children(name="route"))]
    routes: Vec<Route>,
    #[knuffel(children(name="plugin"))]
    plugins: Vec<Plugin>,
}

let config = parse::<Document>("example.kdl", r#"
    version "2.0"
    route "/api" {
        route "/api/v1"
    }
    plugin "http" url="https://example.org/http"
"#)?;
```

See description of [Decode](derive@Decode) and
[DecodeScalar](derive@DecodeScalar) for the full
reference on allowed attributes and parse modes.

# Errors

This crate publishes nice errors, like this:

<img src="https://raw.githubusercontent.com/tailhook/knuffel/main/images/error.png" alt="
Screenshot of error. Here is how narratable printer would print the error:
Error: single char expected after `Alt+`
    Diagnostic severity: error
\
Begin snippet for test.kdl starting at line 17, column 1
\
snippet line 17:     }
snippet line 18:     key &quot;Alt+&quot; mode=&quot;normal&quot; {
    label starting at line 18, column 10: invalid value
snippet line 19:         move-focus &quot;left&quot;
">

To make them working, [miette]'s "fancy" feature must be enabled in the final
application's `Cargo.toml`:
```toml
[dependencies]
miette = { version="3.3.0", features=["fancy"] }
```
And the error returned from parser should be converted to [miette::Report] and
printed with debugging handler. The most manual way to do that is:
```rust
# #[derive(knuffel::Decode, Debug)]
# struct Config {}
# let file_name = "1.kdl";
# let text = "";
let config = match knuffel::parse::<Config>(file_name, text) {
    Ok(config) => config,
    Err(e) => {
         println!("{:?}", miette::Report::new(e));
         std::process::exit(1);
    }
};
```
But usually function that returns `miette::Result` is good enough:
```rust,no_run
# use std::fs;
# #[derive(knuffel::Decode)]
# struct Config {}
use miette::{IntoDiagnostic, Context};

fn parse_config(path: &str) -> miette::Result<Config> {
    let text = fs::read_to_string(path).into_diagnostic()
        .wrap_err_with(|| format!("cannot read {:?}", path))?;
    Ok(knuffel::parse(path, &text)?)
}
fn main() -> miette::Result<()> {
    let config = parse_config("my.kdl")?;
    # Ok(())
}
```

See [miette guide] for other ways of configuring error output.


License
=======

Licensed under either of

* Apache License, Version 2.0,
  (./LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0)
* MIT license (./LICENSE-MIT or http://opensource.org/licenses/MIT)
  at your option.

Contribution
------------

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in the work by you, as defined in the Apache-2.0
license, shall be dual licensed as above, without any additional terms or
conditions.


[specification]: https://github.com/kdl-org/kdl/blob/main/SPEC.md
[miette]: https://docs.rs/miette/
[miette guide]: https://docs.rs/miette/latest/miette/#-handler-options
