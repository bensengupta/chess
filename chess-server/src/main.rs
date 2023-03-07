use std::fmt::{Debug, Write};

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
        use Piece::*;
        f.write_char(match *self {
            King => 'k',
            Queen => 'q',
            Bishop => 'b',
            Knight => 'n',
            Rook => 'r',
            Pawn => 'p',
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

impl Debug for Square {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut piece = format!("{:#?}", self.piece);
        if self.color == Color::White {
            piece = piece.to_ascii_uppercase()
        }
        f.write_str(&piece)
    }
}

impl Square {
    fn parse_fen_placement_char(c: char) -> Result<Self, ()> {
        let color = if c.is_ascii_uppercase() {
            Color::White
        } else {
            Color::Black
        };

        match c.to_ascii_lowercase() {
            'r' => Ok(Square::new(Piece::Rook, color)),
            'n' => Ok(Square::new(Piece::Knight, color)),
            'b' => Ok(Square::new(Piece::Bishop, color)),
            'q' => Ok(Square::new(Piece::Queen, color)),
            'k' => Ok(Square::new(Piece::King, color)),
            'p' => Ok(Square::new(Piece::Pawn, color)),
            _ => Err(()),
        }
    }
}

impl Square {
    fn new(piece: Piece, color: Color) -> Self {
        Self { color, piece }
    }
}

// ============================================
//                  BOARD STRUCTS
// ============================================

struct Pos {
    row: usize,
    col: usize,
}

impl Pos {
    fn new(row: usize, col: usize) -> Self {
        Self { row, col }
    }
}

struct Move {
    from: Pos,
    to: Pos,
    promotion: Option<Piece>,
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

// ============================================
//                  BOARD
// ============================================

struct Board {
    squares: Vec<Vec<Option<Square>>>,
}

impl Board {
    fn new(rows: usize, cols: usize) -> Self {
        Self {
            squares: vec![vec![None; cols]; rows],
        }
    }

    fn get(&self, pos: Pos) -> Option<Square> {
        self.squares[pos.row][pos.col]
    }

    fn parse_fen_placement(fen: String) -> Result<Self, ()> {
        let mut board = Self::new(8, 8);
        let (rows, cols) = (board.squares.len(), board.squares[0].len());

        let chars: Vec<Vec<_>> = fen.split('/').map(|line| line.chars().collect()).collect();

        // Check rows match
        if chars.len() != rows {
            return Err(());
        }

        for (row, line) in chars.into_iter().enumerate() {
            let mut col = 0;
            for c in line {
                if col >= cols {
                    return Err(());
                }
                if c.is_ascii_digit() {
                    col += c.to_digit(10).unwrap() as usize;
                    if col > cols {
                        return Err(());
                    }
                    continue;
                }
                if let Ok(sq) = Square::parse_fen_placement_char(c) {
                    board.squares[row][col] = Some(sq);
                    col += 1;
                    continue;
                }
                return Err(());
            }
            // Check columns match
            if col != cols {
                return Err(());
            }
        }

        Ok(board)
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
        let (rows, cols) = (self.squares.len(), self.squares[0].len());
        for r in 0..rows {
            for c in 0..cols {
                match &self.squares[r][c] {
                    Some(sq) => f.write_fmt(format_args!("{:#?} ", sq))?,
                    None => f.write_str("  ")?,
                }
            }
            f.write_char('\n')?;
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
    ep_square: Pos,
}

fn main() {
    // let game = Game::new();
    // game.moveableSquares() -> Vec<Square>
    // game.moves(Square) -> Vec<Square>
    // game.move(Square, Square) -> Result<()>
    // game.get(Square) -> Piece
    // game.status() -> Status
    let board = Board::default();
    println!("Hello, world!");
    println!("{:#?}", board);
}
