use crate::token::Token;
use std::collections::HashMap;
use std::fmt::Debug;
use crate::position::Position;
use std::borrow::Borrow;


pub struct NewlineHandler<T: Iterator<Item = char>> {
    source: T,
    chr0: Option<char>,
    chr1: Option<char>,
}


impl<T> NewlineHandler<T>
where T : Iterator<Item=char> {
    pub fn new(source: T) -> Self {
        let mut nlh = NewlineHandler {
            source: source,
            chr0: None,
            chr1: None
        };

        nlh.shift();
        nlh.shift();
        nlh
    }

    fn shift(&mut self) -> Option<char> {
        let ret_char = self.chr0;
        self.chr0 = self.chr1;
        self.chr1 = self.source.next();
        ret_char
    }
}


impl<T> Iterator for NewlineHandler<T>
where
    T: Iterator<Item=char>
{
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.chr0 == Some('\r') {
                if self.chr1 == Some('\n') {
                    // Transform windows EOL into \n
                    self.shift();
                } else {
                    // Transform MAC EOL into \n
                    self.chr0 = Some('\n')
                }
            } else {
                break;
            }
        }

        self.shift()
    }
}


pub struct Lexer<T: Iterator<Item = char> + Debug> {
    chars: T,
    chr0: Option<char>,
    chr1: Option<char>,
    keywords: HashMap<String, Token>,
    pos: Position,
    pub result: Vec<Token>,

}


impl<T> Lexer<T>
    where
        T: Iterator<Item = char> + Debug {
    pub fn new(input: T) -> Self {
        let mut lex = Lexer {
            chars: input,
            chr0: None,
            chr1: None,
            keywords: get_keywords(),
            pos: Position::new(1, 1),
            result: vec![]
        };
        lex.next_char();
        lex.next_char();
        lex
    }

    pub fn lex_input(&mut self) {
        loop {
            if self.chr0 == None {
                break;
            }

            match self.chr0.unwrap() {
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut result = Vec::new();
                    loop {
                        if self.chr0 == None || !(
                                self.chr0.unwrap() == '_' ||
                                (self.chr0.unwrap() >= '0' && self.chr0.unwrap() <= '9') ||
                                (self.chr0.unwrap() >= 'a' && self.chr0.unwrap() <= 'z') ||
                                (self.chr0.unwrap() >= 'A' && self.chr0.unwrap() <= 'Z')
                        ) {
                            break;
                        }
                        result.push(self.chr0);
                        self.next_char();
                    }
                    let name = result.iter().map(|&x| x.unwrap().to_string()).collect::<Vec<String>>().join("");
                    if let Some(out) = self.keywords.get(&name) {
                        self.result.push(out.to_owned());
                    } else {
                        self.result.push(Token::Ident(name));
                    }
                }
                _ => {}
            }
        }
    }

    fn next_char(&mut self) -> Option<char> {
        let tmp_char = self.chr0;
        self.chr0 = self.chr1;
        self.chr1 = self.chars.next();
        if tmp_char == Some('\n') {
            self.pos.new_line();
        } else {
            self.pos.go_right();
        }
        tmp_char
    }
}
