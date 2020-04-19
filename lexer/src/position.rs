use std::fmt;

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Position {
    row: usize,
    column: usize,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "line {} column {}", self.row, self.column)
    }
}

impl Position {
    pub fn new(row: usize, column: usize) -> Self {
        Position {
            row,
            column
        }
    }

    pub fn reset(&mut self) {
        self.row = 1;
        self.column = 1;
    }

    pub fn go_right(&mut self) {
        self.column += 1;
    }

    pub fn new_line(&mut self) {
        self.row += 1;
        self.column = 1;
    }

}
