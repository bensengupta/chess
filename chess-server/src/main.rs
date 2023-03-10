use std::fmt::{Debug, Write};

enum ParseError {
    InvalidCharacter,
    InvalidDimensions,
}

impl Debug for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ParseError::*;
        f.write_str(match *self {
            InvalidCharacter => "invalid character",
            InvalidDimensions => "invalid dimensions",
        })
    }
}

struct IllegalMoveError;

impl Debug for IllegalMoveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("illegal move")
    }
}

struct InvalidPosError;

impl Debug for InvalidPosError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("invalid position")
    }
}

// ============================================
//                  PIECE
// ============================================

#[derive(Clone, Copy)]
enum Piece {
    King,
    Queen,
    Bishop,
    Knight,
    Rook,
    Pawn,
}

impl Debug for Piece {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(match *self {
            Piece::King => 'k',
            Piece::Queen => 'q',
            Piece::Bishop => 'b',
            Piece::Knight => 'n',
            Piece::Rook => 'r',
            Piece::Pawn => 'p',
        })
    }
}

// ============================================
//                  COLOR
// ============================================

#[derive(Clone, Copy, PartialEq)]
enum Color {
    White,
    Black,
}

// ============================================
//                  SQUARE
// ============================================

#[derive(Clone, Copy)]
struct Square {
    piece: Piece,
    color: Color,
}

impl Square {
    fn new(piece: Piece, color: Color) -> Self {
        Self { color, piece }
    }
}

impl Square {
    fn parse_fen_placement_char(c: char) -> Result<Self, ParseError> {
        let color = match c.is_ascii_uppercase() {
            true => Color::White,
            false => Color::Black,
        };

        let piece = match c.to_ascii_lowercase() {
            'r' => Piece::Rook,
            'n' => Piece::Knight,
            'b' => Piece::Bishop,
            'q' => Piece::Queen,
            'k' => Piece::King,
            'p' => Piece::Pawn,
            _ => return Err(ParseError::InvalidCharacter),
        };

        Ok(Self::new(piece, color))
    }
}

impl Debug for Square {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.color {
            Color::White => f.write_str(format!("{:#?}", self.piece).to_uppercase().as_str()),
            Color::Black => self.piece.fmt(f),
        }
    }
}

// ============================================
//                  BOARD STRUCTS
// ============================================

#[derive(Clone, Copy)]
struct Pos {
    row: usize,
    col: usize,
}

impl Pos {
    fn new(row: usize, col: usize) -> Self {
        Self { row, col }
    }
    fn algebraic(s: String) -> Result<Self, InvalidPosError> {
        let mut chars = s.chars();
        let row = 7 - "abcdefgh"
            .find(chars.next().ok_or(InvalidPosError)?)
            .ok_or(InvalidPosError)?;
        let col = "12345678"
            .find(chars.next().ok_or(InvalidPosError)?)
            .ok_or(InvalidPosError)?;

        Ok(Self { row, col })
    }
}

struct Move {
    from: Pos,
    to: Pos,
    promotion: Option<Piece>,
}

impl Move {
    fn new(from: Pos, to: Pos) -> Self {
        Self {
            from,
            to,
            promotion: None,
        }
    }
    fn promote(mut self, piece: Piece) -> Self {
        self.promotion = Some(piece);
        self
    }
}

enum Status {
    WaitingForTurn(Color),
    InCheck(Color),
    Checkmate(Color),
    Draw,
}

struct Castling {
    white_queenside: bool,
    white_kingside: bool,
    black_queenside: bool,
    black_kingside: bool,
}

impl Default for Castling {
    fn default() -> Self {
        Self {
            white_queenside: true,
            white_kingside: true,
            black_queenside: true,
            black_kingside: true,
        }
    }
}

// ============================================
//                  BOARD
// ============================================

struct Board {
    squares: Vec<Vec<Option<Square>>>,
}

impl Board {
    fn new(rows: usize, cols: usize) -> Self {
        if rows == 0 || cols == 0 {
            panic!("board rows/cols cannot be 0");
        }
        Self {
            squares: vec![vec![None; cols]; rows],
        }
    }

    fn dimensions(&self) -> (usize, usize) {
        (self.squares.len(), self.squares[0].len())
    }

    fn parse_fen_placement(fen: String) -> Result<Self, ParseError> {
        let mut board = Board::new(8, 8);
        let (rows, cols) = Board::dimensions(&board);

        let chars: Vec<Vec<_>> = fen.split('/').map(|line| line.chars().collect()).collect();

        // Check rows match
        if chars.len() != rows {
            return Err(ParseError::InvalidDimensions);
        }

        for (row, line) in chars.into_iter().enumerate() {
            let mut col = 0;
            for c in line {
                if col >= cols {
                    return Err(ParseError::InvalidDimensions);
                }
                match c {
                    c if c.is_ascii_digit() => {
                        col += c.to_digit(10).unwrap() as usize;
                    }
                    c => {
                        board.squares[row][col] = Some(Square::parse_fen_placement_char(c)?);
                        col += 1;
                    }
                }
            }
            // Check columns match
            if col != cols {
                return Err(ParseError::InvalidDimensions);
            }
        }

        Ok(board)
    }

    fn get(&self, pos: Pos) -> Option<Square> {
        self.squares[pos.row][pos.col]
    }

    fn set(&mut self, pos: Pos, sq: Option<Square>) {
        self.squares[pos.row][pos.col] = sq;
    }

    fn is_in_bounds(&self, pos: Pos) -> bool {
        let (rows, cols) = self.dimensions();
        pos.col < rows && pos.row < cols
    }
}

impl Default for Board {
    fn default() -> Self {
        let default_placement = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR";

        Self::parse_fen_placement(default_placement.to_string()).unwrap()
    }
}

impl Debug for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (rows, cols) = self.dimensions();
        for r in 0..rows {
            for c in 0..cols {
                match &self.squares[r][c] {
                    Some(sq) => sq.fmt(f)?,
                    None => f.write_char(' ')?,
                }
                f.write_char(' ')?;
            }
            if r != rows - 1 {
                f.write_char('\n')?;
            }
        }

        Ok(())
    }
}

// ============================================
//                  GAME
// ============================================

struct Game {
    board: Board,
    turn: Color,
    castling: Castling,
    ep_square: Option<Pos>,
}

impl Default for Game {
    fn default() -> Self {
        Self {
            board: Board::default(),
            turn: Color::White,
            castling: Castling::default(),
            ep_square: None,
        }
    }
}

impl Game {
    fn make_move(&mut self, mov: Move) -> Result<(), IllegalMoveError> {
        let board = &mut self.board;

        if !board.is_in_bounds(mov.from) || !board.is_in_bounds(mov.to) {
            return Err(IllegalMoveError);
        }

        board.set(mov.to, board.get(mov.from));
        board.set(mov.from, None);

        Ok(())
    }
}

fn main() {
    // let game = Game::new();
    // game.moveableSquares() -> Vec<Square>
    // game.moves(Square) -> Vec<Square>
    // game.move(Square, Square) -> Result<()>
    // game.get(Square) -> Piece
    // game.status() -> Status
    let mut game = Game::default();

    game.make_move(Move::new(
        Pos::algebraic("a1".to_string()).unwrap(),
        Pos::algebraic("a2".to_string()).unwrap(),
    ))
    .unwrap();

    // let board = Board::default();
    println!("{:#?}", game.board);
}
