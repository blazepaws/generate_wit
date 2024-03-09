use chumsky::prelude::{choice, just, one_of, Parser};

pub fn lex_semver<'a>() -> impl Parser<'a, &'a str, &'a str> {

    let letter = one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVW");
    let positive_digit = one_of("123456789");
    let digit = choice((
        positive_digit.to_slice(),
        just("0")
    ));
    let digits = digit.repeated().at_least(1).to_slice();
    let non_digit = choice((letter.to_slice(), just("-")));
    let identifier_character = digit.or(non_digit);
    let identifier_characters = identifier_character.repeated().at_least(1).to_slice();
    let numeric_identifier = choice((
        just("0"),
        positive_digit.then(digits.or_not()).to_slice(),
    ));
    let alphanumeric_identifier = choice((
        non_digit.then(identifier_characters.or_not()).to_slice(),
        identifier_characters.then(non_digit).to_slice(),
        identifier_characters.then(non_digit).then(identifier_characters).to_slice()
    ));
    let build_identifier = choice((
        alphanumeric_identifier,
        digits
    ));
    let pre_release_identifier = choice((
        alphanumeric_identifier,
        numeric_identifier
    ));
    let dot_separated_build_identifiers = build_identifier
        .separated_by(just("."))
        .at_least(1)
        .to_slice();
    let build = dot_separated_build_identifiers;

    let dot_separated_pre_release_identifiers = pre_release_identifier
        .separated_by(just("."))
        .at_least(1)
        .to_slice();

    let pre_release = dot_separated_pre_release_identifiers;
    let patch = numeric_identifier;
    let minor = numeric_identifier;
    let major = numeric_identifier;
    let version_core = major
        .then(just(".").then(minor))
        .then(just(".").then(patch));

    version_core
        .then(just("-").then(pre_release).or_not())
        .then(just("+").then(build).or_not())
        .to_slice()
        .boxed()
}

pub fn parse_semver<'a>() -> impl Parser<'a, &'a str, semver::Version> {
    lex_semver().map(|s| {
        semver::Version::parse(s)
            .expect("Implementation error: The semver parser should match the semver crate's parser exactly!")
    })
    .boxed()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid() {
        assert_eq!(
            parse_semver().parse("1.2.3").unwrap(),
            semver::Version::parse("1.2.3").unwrap()
        );
        assert_eq!(
            parse_semver().parse("1.0.0-pre+build").unwrap(),
            semver::Version::parse("1.0.0-pre+build").unwrap()
        );
        assert_eq!(
            parse_semver().parse("1.0.0-pre").unwrap(),
            semver::Version::parse("1.0.0-pre").unwrap()
        );
        assert_eq!(
            parse_semver().parse("1.0.0+build").unwrap(),
            semver::Version::parse("1.0.0+build").unwrap()
        );
        // A bit stupid, but it does match the spec.
        // The part after the - is not a pre, but simply part of the build.
        assert_eq!(
            parse_semver().parse("1.0.0+build-notpre").unwrap(),
            semver::Version::parse("1.0.0+build-notpre").unwrap()
        );
    }

    #[test]
    fn test_invalid() {
        assert!(parse_semver().parse("").has_errors());
        assert!(parse_semver().parse("1.2").has_errors());
        assert!(parse_semver().parse("1").has_errors());
        assert!(parse_semver().parse("1.2.").has_errors());
        assert!(parse_semver().parse("1.2.3.").has_errors());
        assert!(parse_semver().parse("+a").has_errors());
        assert!(parse_semver().parse("-a").has_errors());
    }
}
