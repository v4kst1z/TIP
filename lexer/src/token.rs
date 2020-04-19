use std::collections::HashMap;
use crate::position::Position;

#[derive(Clone,Debug, Eq, Hash, PartialEq)]
pub enum Token {
    If,
    Else,
    While,
    Return,
    Var,
    Input,
    Output,
    Alloc,
    Null,
    Error,

    Ident(String),

    //number
    Number(u64),

    /* Delimiters */
    Lparen,    //(
    Rparen,    //)
    Lcurly,    //{
    Rcurly,    //}
    Semi,      //;
    Comma,      //,

    /* Assignment */
    Assign,     //=

    /* Comparison  */
    Equal,      //==
    Great,      //>

    /* Arithmetic */
    Plus,       //+
    Sub,         // -
    Mult,        // *
    Div,          // /
    Ampersand,    //&

    Eof,
}

fn get_keywords() -> HashMap<String, Token> {
    let mut result = HashMap::new();

    result.insert(String::from("if"), Token::If);
    result.insert(String::from("else"), Token::Else);
    result.insert(String::from("while"), Token::While);
    result.insert(String::from("return"), Token::Return);
    result.insert(String::from("var"), Token::Var);
    result.insert(String::from("input"), Token::Input);
    result.insert(String::from("output"), Token::Output);
    result.insert(String::from("alloc"), Token::Alloc);
    result.insert(String::from("null"), Token::Null);
    result.insert(String::from("error"), Token::Error);

    result
}