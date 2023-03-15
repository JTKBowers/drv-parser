use std::collections::HashMap;

use nom::{
    bytes::complete::{escaped, is_not, tag},
    character::complete::{char, none_of, one_of},
    combinator::opt,
    multi::{separated_list0, separated_list1},
    sequence::{delimited, terminated, tuple},
    IResult,
};

#[derive(Debug)]
pub struct Output<'a> {
    pub name: &'a str,
    pub derivation: &'a str,
}

#[derive(Debug)]
pub struct Builder<'a> {
    pub derivation: &'a str,
    pub arguments: Vec<&'a str>,
}
#[derive(Debug)]
pub struct Derivation<'a> {
    pub platform: &'a str,
    pub outputs: Vec<Output<'a>>,
    pub builder: Builder<'a>,
    pub attributes: HashMap<&'a str, Option<&'a str>>,
    pub dependencies: Vec<Dependency<'a>>,
    pub build_dependencies: Vec<&'a str>,
}

fn parse_string(input: &str) -> IResult<&str, Option<&str>> {
    delimited(
        char('"'),
        opt(escaped(none_of("\"\\"), '\\', one_of("\"\\nt"))),
        char('"'),
    )(input)
}
fn parse_required_string(input: &str) -> IResult<&str, &str> {
    delimited(char('"'), is_not("\""), char('"'))(input)
}

fn parse_output(input: &str) -> IResult<&str, Output> {
    let (input, (output_name, derivation, _, _)) = delimited(
        char('('),
        tuple((
            terminated(parse_required_string, char(',')),
            terminated(parse_required_string, char(',')),
            terminated(parse_string, char(',')),
            parse_string,
        )),
        char(')'),
    )(input)?;
    Ok((
        input,
        Output {
            name: output_name,
            derivation,
        },
    ))
}

#[derive(Debug)]
pub struct Dependency<'a> {
    pub derivation: &'a str,
    pub outputs: Vec<&'a str>,
}
fn parse_dependency(input: &str) -> IResult<&str, Dependency> {
    let (input, (derivation, outputs)) = delimited(
        char('('),
        tuple((
            terminated(parse_required_string, char(',')),
            delimited(
                char('['),
                separated_list1(char(','), parse_required_string),
                char(']'),
            ),
        )),
        char(')'),
    )(input)?;
    Ok((
        input,
        Dependency {
            derivation,
            outputs,
        },
    ))
}
pub fn parse_drv(input: &str) -> IResult<&str, Derivation> {
    let (input, _) = tag("Derive(")(input)?;
    let (input, outputs) = terminated(
        delimited(
            char('['),
            separated_list0(char(','), parse_output),
            char(']'),
        ),
        char(','),
    )(input)?;
    let (input, dependencies) = terminated(
        delimited(
            char('['),
            separated_list0(char(','), parse_dependency),
            char(']'),
        ),
        char(','),
    )(input)?;
    let (input, build_dependencies) = delimited(
        char('['),
        separated_list0(char(','), parse_required_string),
        char(']'),
    )(input)?;
    let (input, _) = char(',')(input)?;
    let (input, platform) = terminated(parse_required_string, char(','))(input)?;
    let (input, builder_derivation) = terminated(parse_required_string, char(','))(input)?;
    let (input, builder_arguments) = terminated(
        delimited(
            char('['),
            separated_list0(char(','), parse_required_string),
            char(']'),
        ),
        char(','),
    )(input)?;
    let builder = Builder {
        derivation: builder_derivation,
        arguments: builder_arguments,
    };
    let (input, drv_attributes) = delimited(
        char('['),
        separated_list0(
            char(','),
            delimited(
                char('('),
                tuple((terminated(parse_required_string, char(',')), parse_string)),
                char(')'),
            ),
        ),
        char(']'),
    )(input)?;
    let attributes = drv_attributes
        .into_iter()
        .collect::<HashMap<&str, Option<&str>>>();
    let (input, _) = tag(")")(input)?;
    Ok((
        input,
        Derivation {
            platform,
            outputs,
            builder,
            attributes,
            dependencies,
            build_dependencies,
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_can_parse_an_unescaped_string() {
        let (remaining_input, contents) = parse_string("\"abc\"").unwrap();
        assert_eq!(remaining_input, "");
        assert_eq!(contents, Some("abc"));
    }

    #[test]
    fn it_can_parse_an_escaped_string() {
        let (remaining_input, contents) = parse_string(r#""ab\\c\"""#).unwrap();
        assert_eq!(remaining_input, "");
        assert_eq!(contents, Some(r#"ab\\c\""#));
    }

    #[test]
    fn it_can_parse_an_output_block() {
        let (remaining_input, block) = parse_output(
            "(\"dev\",\"/nix/store/8zhl01sb1gjxlfmvxxacpiafzvah1p9l-brotli-1.0.9-dev\",\"\",\"\")",
        )
        .unwrap();
        assert_eq!(remaining_input, "");
        assert_eq!(block.name, "dev");
        assert_eq!(
            block.derivation,
            "/nix/store/8zhl01sb1gjxlfmvxxacpiafzvah1p9l-brotli-1.0.9-dev",
        )
    }

    #[test]
    fn it_works() {
        let (remaining_input, derivation) = parse_drv(include_str!("example.drv")).unwrap();
        println!("{:?}", derivation);
        assert_eq!(remaining_input, "");
    }
}
