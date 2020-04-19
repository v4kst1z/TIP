#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}


pub mod error;
pub mod lexer;
pub mod position;
pub mod token;