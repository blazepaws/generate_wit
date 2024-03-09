/// Based on the spec found at
/// https://github.com/WebAssembly/component-model/blob/main/design/mvp/WIT.md#lexical-structure
/// Last updated at commit 9cda856.
///

use std::fmt;
use std::fmt::Formatter;
use chumsky::{extra, IterParser, Parser, ParseResult};
use chumsky::input::Input;
use chumsky::prelude::{any, choice, just, none_of, one_of, recursive, Rich};
use chumsky::span::SimpleSpan;
use serde::{Deserialize, Serialize};

#[derive(Eq, PartialEq, Copy, Clone, Serialize, Deserialize)]
pub enum Operator {
    Equals,
    Comma,
    Colon,
    Semicolon,
    ParenLeft,
    ParenRight,
    BraceLeft,
    BraceRight,
    AngleLeft,
    AngleRight,
    Star,
    Arrow,
    Slash,
    Dot,
    At,
}

impl fmt::Debug for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("\"{}\"", self.to_string()))
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            Operator::Equals => "=",
            Operator::Comma => ",",
            Operator::Colon => ":",
            Operator::Semicolon => ";",
            Operator::ParenLeft => "(",
            Operator::ParenRight => ")",
            Operator::BraceLeft => "{",
            Operator::BraceRight => "}",
            Operator::AngleLeft => "<",
            Operator::AngleRight => ">",
            Operator::Star => "*",
            Operator::Arrow => "->",
            Operator::Slash => "/",
            Operator::Dot => ".",
            Operator::At => "@",
        };
        write!(f, "{}", s)
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Serialize, Deserialize)]
pub enum Keyword {
    Use,
    Type,
    Resource,
    Func,
    Record,
    Enum,
    Flags,
    Variant,
    Static,
    Interface,
    World,
    Import,
    Export,
    Package,
    Include,
}

impl fmt::Debug for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("\"{}\"", self.to_string()))
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            Keyword::Use => "use",
            Keyword::Type => "type",
            Keyword::Resource => "resource",
            Keyword::Func => "func",
            Keyword::Record => "record",
            Keyword::Enum => "enum",
            Keyword::Flags => "flags",
            Keyword::Variant => "variant",
            Keyword::Static => "static",
            Keyword::Interface => "interface",
            Keyword::World => "world",
            Keyword::Import => "import",
            Keyword::Export => "export",
            Keyword::Package => "package",
            Keyword::Include => "include",
        };
        write!(f, "{}", s)
    }
}

#[derive(Eq, PartialEq, Debug, Serialize, Deserialize)]
pub enum Token<'a> {
    Whitespace(&'a str),
    /// I made a decision to separate whitespace from comments, which the spec doesn't do.
    /// This doesn't change anything syntactically or semantically.
    /// Comments should be treated exactly like whitespace.
    /// Sometimes it can be useful to have comments separated already (like parsing docs).
    Comment(&'a str),
    Operator(Operator),
    Keyword(Keyword),
    Integer(i32),
    Identifier(&'a str),
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Token::Whitespace(s) => write!(f, "{}", s),
            Token::Comment(s) => write!(f, "{}", s),
            Token::Operator(op) => write!(f, "{}", op),
            Token::Keyword(kw) => write!(f, "{}", kw),
            Token::Integer(i) => write!(f, "{}", i),
            Token::Identifier(s) => write!(f, "{}", s),
        }
    }
}

impl<'a> Token<'a> {
    fn to_owned_token(&self) -> OwnedToken {
        match self {
            Token::Whitespace(s) => OwnedToken::Whitespace((*s).to_owned()),
            Token::Comment(s) => OwnedToken::Comment((*s).to_owned()),
            Token::Operator(op) => OwnedToken::Operator(*op),
            Token::Keyword(kw) => OwnedToken::Keyword(*kw),
            Token::Integer(i) => OwnedToken::Integer(*i),
            Token::Identifier(id) => OwnedToken::Identifier((*id).to_owned()),
        }
    }
}

/// Like token, but owns its contents instead of referencing into a lexed string.
/// This is useful for generating wit files.
#[derive(Eq, PartialEq, Debug, Clone, Serialize, Deserialize)]
pub enum OwnedToken {
    Whitespace(String),
    /// I made a decision to separate whitespace from comments, which the spec doesn't do.
    /// This doesn't change anything syntactically or semantically.
    /// Comments should be treated exactly like whitespace.
    /// Sometimes it can be useful to have comments separated already (like parsing docs).
    Comment(String),
    Operator(Operator),
    Keyword(Keyword),
    Integer(i32),
    Identifier(String),
}

impl fmt::Display for OwnedToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OwnedToken::Whitespace(s) => write!(f, "{}", s),
            OwnedToken::Comment(s) => write!(f, "{}", s),
            OwnedToken::Operator(op) => write!(f, "{}", op),
            OwnedToken::Keyword(kw) => write!(f, "{}", kw),
            OwnedToken::Integer(i) => write!(f, "{}", i),
            OwnedToken::Identifier(s) => write!(f, "{}", s),
        }
    }
}

fn keyword<'a>() -> impl Parser<'a, &'a str, Token<'a>, extra::Err<Rich<'a, char>>> {
    choice((
        just("use").to(Keyword::Use),
        just("type").to(Keyword::Type),
        just("resource").to(Keyword::Resource),
        just("func").to(Keyword::Func),
        just("record").to(Keyword::Record),
        just("enum").to(Keyword::Enum),
        just("flags").to(Keyword::Flags),
        just("variant").to(Keyword::Variant),
        just("static").to(Keyword::Static),
        just("interface").to(Keyword::Interface),
        just("world").to(Keyword::World),
        just("import").to(Keyword::Import),
        just("export").to(Keyword::Export),
        just("package").to(Keyword::Package),
        just("include").to(Keyword::Include),
    )).map(|k| Token::Keyword(k))
}

fn integer<'a>() -> impl Parser<'a, &'a str, Token<'a>, extra::Err<Rich<'a, char>>> {
    one_of("0123456789")
        .repeated()
        .at_least(1)
        .to_slice()
        .try_map(|s: &str, span| {
            let i: i32 = s.parse().map_err(|_| Rich::custom(span, "Not an integer"))?;
            Ok(Token::Integer(i))
        })
}

fn whitespace<'a>() -> impl Parser<'a, &'a str, Token<'a>, extra::Err<Rich<'a, char>>> {
    // To reduce token spam we don't create a new whitespace token for each whitespace
    // character. If you care about it, you can parse the token's string.
    one_of(" \n\r\t")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(|s| Token::Whitespace(s))
}

fn operator<'a>() -> impl Parser<'a, &'a str, Token<'a>, extra::Err<Rich<'a, char>>> {
    choice((
        just("=").to(Operator::Equals),
        just(",").to(Operator::Comma),
        just(":").to(Operator::Colon),
        just(";").to(Operator::Semicolon),
        just("(").to(Operator::ParenLeft),
        just(")").to(Operator::ParenRight),
        just("{").to(Operator::BraceLeft),
        just("}").to(Operator::BraceRight),
        just("<").to(Operator::AngleLeft),
        just(">").to(Operator::AngleRight),
        just("*").to(Operator::Star),
        just("->").to(Operator::Arrow),
        just("/").to(Operator::Slash),
        just(".").to(Operator::Dot),
        just("@").to(Operator::At),
    )).map(|op| Token::Operator(op))
}

/// See: https://github.com/WebAssembly/component-model/blob/main/design/mvp/Explainer.md#import-and-export-definitions
fn word<'a>() -> impl Parser<'a, &'a str, &'a str, extra::Err<Rich<'a, char>>> {
    let letter = one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVW");
    let digit = one_of("123456789");
    let alpha_num = letter.or(digit);
    letter.then(alpha_num.repeated()).to_slice()
}

/// See: https://github.com/WebAssembly/component-model/blob/main/design/mvp/Explainer.md#import-and-export-definitions
fn label<'a>() -> impl Parser<'a, &'a str, &'a str, extra::Err<Rich<'a, char>>> {
    word().then(just("-").then(word()).repeated()).to_slice()
}

/// See: https://github.com/WebAssembly/component-model/blob/main/design/mvp/Explainer.md#import-and-export-definitions
fn plain_name<'a>() -> impl Parser<'a, &'a str, &'a str, extra::Err<Rich<'a, char>>> {
    choice((
        label(),
        just("[constructor]").then(label()).to_slice(),
        just("[method]").then(label()).then(just(".")).then(label()).to_slice(),
        just("[static]").then(label()).then(just(".")).then(label()).to_slice(),
    ))
}

/// See: https://github.com/WebAssembly/component-model/blob/main/design/mvp/Explainer.md#import-and-export-definitions
fn interface_name<'a>() -> impl Parser<'a, &'a str, &'a str, extra::Err<Rich<'a, char>>> {
    namespace().repeated().at_least(1)
        .then(label())
        .then(projection().repeated().at_least(1))
        .then(version().or_not())
        .to_slice()
}

/// See: https://github.com/WebAssembly/component-model/blob/main/design/mvp/Explainer.md#import-and-export-definitions
fn namespace<'a>() -> impl Parser<'a, &'a str, &'a str, extra::Err<Rich<'a, char>>> {
    label().then(just(":")).to_slice()
}

/// See: https://github.com/WebAssembly/component-model/blob/main/design/mvp/Explainer.md#import-and-export-definitions
fn projection<'a>() -> impl Parser<'a, &'a str, &'a str, extra::Err<Rich<'a, char>>> {
    just("/").then(label()).to_slice()
}

/// See: https://github.com/WebAssembly/component-model/blob/main/design/mvp/Explainer.md#import-and-export-definitions
fn version<'a>() -> impl Parser<'a, &'a str, &'a str, extra::Err<Rich<'a, char>>> {
    just("@")
        .then(chumsky_semver::lex_semver())
        .to_slice()
        .boxed() // this is quite complex, prevent the compiler from freaking out
}

fn line_comment<'a>() -> impl Parser<'a, &'a str, &'a str, extra::Err<Rich<'a, char>>> {
    just("//").then(none_of("\n").repeated()).to_slice()
}

fn block_comment<'a>() -> impl Parser<'a, &'a str, &'a str, extra::Err<Rich<'a, char>>> {

    let not_special = just("/*").or(just("*/")).not();
    let text = any()
        .and_is(not_special)
        .repeated()
        .at_least(1)
        .to_slice();

    let block_inner = recursive(|expr| {
        let block = expr
            .delimited_by(just("/*"), just("*/"))
            .to_slice();

        block.or(text).repeated().to_slice()
    });

    just("/*")
        .then(block_inner)
        .then(just("*/"))
        .to_slice()
}

fn comment<'a>() -> impl Parser<'a, &'a str, Token<'a>, extra::Err<Rich<'a, char>>> {
    choice((
        line_comment(),
        block_comment(),
    )).map(|s| Token::Comment(s))
}

fn identifier<'a>() -> impl Parser<'a, &'a str, Token<'a>, extra::Err<Rich<'a, char>>> {
    plain_name()
        .or(interface_name())
        .map(|s| Token::Identifier(s))
        .boxed() // This is quite complex, prevent the compiler from freaking out
}

/// Lex a single wit token
pub fn lex_token<'a>() -> impl Parser<'a, &'a str, (Token<'a>, SimpleSpan), extra::Err<Rich<'a, char>>> {
    choice((
        whitespace(),
        comment(),
        operator(),
        keyword(),
        integer(),
        identifier(),
    )).map_with(|token, extra| {
        (token, extra.span())
    })
}

/// Lex a full wit file
pub fn lex_wit<'a>() -> impl Parser<'a, &'a str, Vec<(Token<'a>, SimpleSpan)>, extra::Err<Rich<'a, char>>> {
    lex_token().repeated().collect()
}

/// Lex a string into a list of tokens
pub fn lex<'a>(s: &'a str) -> ParseResult<Vec<(Token<'a>, SimpleSpan)>, Rich<'a, char>> {
    lex_wit().parse(s)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_identifier() {
        assert_eq!(
            identifier().parse("a").unwrap(),
            Token::Identifier("a")
        );
        assert_eq!(
            identifier().parse("abc-def-ghi").unwrap(),
            Token::Identifier("abc-def-ghi")
        );
        assert_eq!(
            identifier().then_ignore(any().repeated()).parse("a(").unwrap(),
            Token::Identifier("a")
        );
        assert_eq!(
            identifier().then_ignore(any().repeated()).parse("a<").unwrap(),
            Token::Identifier("a")
        );
        assert_eq!(
            identifier().then_ignore(any().repeated()).parse("a b").unwrap(),
            Token::Identifier("a")
        );
        assert_eq!(
            identifier().parse("documentation").unwrap(),
            Token::Identifier("documentation")
        )
    }

    #[test]
    fn test_comment() {
        assert_eq!(
            lex_token().parse("// aaa").unwrap(),
            (Token::Comment("// aaa"), SimpleSpan::new(0, 6))
        );
        assert_eq!(
            lex_token().parse("//aaa").unwrap(),
            (Token::Comment("//aaa"), SimpleSpan::new(0, 5))
        );
        assert_eq!(
            lex_token().parse("/*aaa*/").unwrap(),
            (Token::Comment("/*aaa*/"), SimpleSpan::new(0, 7))
        );
        assert_eq!(
            lex_token().parse("/**/").unwrap(),
            (Token::Comment("/**/"), SimpleSpan::new(0, 4))
        );
        assert_eq!(
            lex_token().parse("/*/**/*/").unwrap(),
            (Token::Comment("/*/**/*/"), SimpleSpan::new(0, 8))
        );
        assert_eq!(
            lex_token().parse("/*a/*a*/a*/").unwrap(),
            (Token::Comment("/*a/*a*/a*/"), SimpleSpan::new(0, 11))
        );
    }

    #[test]
    fn test_operator() {
        assert_eq!(
            lex_token().parse(":").unwrap(),
            (Token::Operator(Operator::Colon), SimpleSpan::new(0, 1))
        );
    }

    #[test]
    fn bug_01() {
        // Reproduces a bug where the order of precedence messed up parsing.
        let wit = "package documentation:http@1.0.0;";
        let tokens = lex(wit).unwrap();
        assert_eq!(tokens, vec![
            (Token::Keyword(Keyword::Package), SimpleSpan::new(0, 7)),
            (Token::Whitespace(" "), SimpleSpan::new(7, 8)),
            (Token::Identifier("documentation"), SimpleSpan::new(8, 21)),
            (Token::Operator(Operator::Colon), SimpleSpan::new(21, 22)),
            (Token::Identifier("http"), SimpleSpan::new(22, 26)),
            (Token::Operator(Operator::At), SimpleSpan::new(26, 27)),
            (Token::Integer(1), SimpleSpan::new(27, 28)),
            (Token::Operator(Operator::Dot), SimpleSpan::new(28, 29)),
            (Token::Integer(0), SimpleSpan::new(29, 30)),
            (Token::Operator(Operator::Dot), SimpleSpan::new(30, 31)),
            (Token::Integer(0), SimpleSpan::new(31, 32)),
            (Token::Operator(Operator::Semicolon), SimpleSpan::new(32, 33))
        ]);
    }

    #[test]
    fn simple_fulltext_roundtrip() {
        let wit = "
        package documentation:http@1.0.0;

        world proxy {
            export incoming-handler;
            import outgoing-handler;
        }
        ";
        let tokens = lex(wit).unwrap();
        let mut reencoded = String::new();
        for (token, _span) in tokens {
            reencoded += token.to_string().as_str();
        }
        assert_eq!(wit, reencoded.as_str());
    }
}