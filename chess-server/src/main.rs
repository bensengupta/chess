use std::{
    fmt::{Debug, Write},
    ops::Not,
    str::FromStr,
};

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

#[derive(Clone, Copy, PartialEq)]
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

impl Piece {
    #[rustfmt::skip]
    fn offsets(&self) -> Vec<(i32, i32)> {
        match *self {
            Piece::King => vec![(-1, -1), (-1, 0), (-1,  1),
                                ( 0, -1),          ( 0,  1),
                                ( 1, -1), ( 1, 0), ( 1,  1)],

            Piece::Queen => vec![(-1, -1), (-1, 0), (-1,  1),
                                 ( 0, -1),          ( 0,  1),
                                 ( 1, -1), ( 1, 0), ( 1,  1)],
                                
            Piece::Bishop => vec![(-1, -1),         (-1,  1),

                                  ( 1, -1),         ( 1,  1)],
            
            Piece::Rook => vec![          (-1, 0),
                                ( 0, -1),          ( 0,  1),
                                          ( 1, 0)           ],

            Piece::Knight => vec![          (-2, -1),           (-2,  1),
                                  (-1, -2),                               (-1,  2),
                                  
                                  ( 1, -2),                               ( 1,  2),
                                            ( 2, -1),           ( 2,  1)],

            Piece::Pawn => vec![(1, 0),
                                (2, 0)]
        }
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

impl Not for Color {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Color::Black => Color::White,
            Color::White => Color::Black,
        }
    }
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

    fn fen(&self) -> String {
        let piece = format!("{:#?}", self.piece);
        match self.color {
            Color::White => piece.to_uppercase(),
            Color::Black => piece,
        }
    }
}

impl Debug for Square {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.fen().as_str())
    }
}

// ============================================
//                  BOARD STRUCTS
// ============================================

#[rustfmt::skip]
#[derive(Clone, Copy, PartialEq)]
enum Pos {
    A8, B8, C8, D8, E8, F8, G8, H8,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A1, B1, C1, D1, E1, F1, G1, H1,
}

impl TryFrom<(usize, usize)> for Pos {
    type Error = ();

    fn try_from((row, col): (usize, usize)) -> Result<Self, Self::Error> {
        use Pos::*;
        let grid = [
            [A8, B8, C8, D8, E8, F8, G8, H8],
            [A7, B7, C7, D7, E7, F7, G7, H7],
            [A6, B6, C6, D6, E6, F6, G6, H6],
            [A5, B5, C5, D5, E5, F5, G5, H5],
            [A4, B4, C4, D4, E4, F4, G4, H4],
            [A3, B3, C3, D3, E3, F3, G3, H3],
            [A2, B2, C2, D2, E2, F2, G2, H2],
            [A1, B1, C1, D1, E1, F1, G1, H1],
        ];

        if row < grid.len() && col < grid[0].len() {
            Ok(grid[row][col])
        } else {
            Err(())
        }
    }
}

impl TryFrom<(i32, i32)> for Pos {
    type Error = ();

    fn try_from((row, col): (i32, i32)) -> Result<Self, Self::Error> {
        if row < 0 || col < 0 {
            Err(())
        } else {
            (row as usize, col as usize).try_into()
        }
    }
}

impl From<Pos> for (usize, usize) {
    fn from(pos: Pos) -> Self {
        (pos.row(), pos.col())
    }
}

impl FromStr for Pos {
    type Err = ();

    fn from_str(input: &str) -> Result<Pos, Self::Err> {
        let mut chars = input.chars();
        let col_char = chars.next().ok_or(())?;
        let row_char = chars.next().ok_or(())?;
        let col = "ABCDEFGH".find(col_char).ok_or(())?;
        let row = "87654321".find(row_char).ok_or(())?;

        (row, col).try_into()
    }
}

impl Pos {
    // 1 = Rank 1
    // 8 = Rank 8
    fn rank(self, color: Color) -> usize {
        let (row, _) = self.into();

        match color {
            Color::Black => row + 1,
            Color::White => 8 - row,
        }
    }

    fn with_offsets(&self, offsets: &Vec<(i32, i32)>) -> Vec<Pos> {
        let (row, col) = (self.row() as i32, self.col() as i32);
        offsets
            .into_iter()
            .map(|(dr, dc)| Pos::try_from((row + dr, col + dc)))
            .filter_map(|x| x.ok())
            .collect()
    }

    fn row(self) -> usize {
        use Pos::*;

        match self {
            A8 | B8 | C8 | D8 | E8 | F8 | G8 | H8 => 0,
            A7 | B7 | C7 | D7 | E7 | F7 | G7 | H7 => 1,
            A6 | B6 | C6 | D6 | E6 | F6 | G6 | H6 => 2,
            A5 | B5 | C5 | D5 | E5 | F5 | G5 | H5 => 3,
            A4 | B4 | C4 | D4 | E4 | F4 | G4 | H4 => 4,
            A3 | B3 | C3 | D3 | E3 | F3 | G3 | H3 => 5,
            A2 | B2 | C2 | D2 | E2 | F2 | G2 | H2 => 6,
            A1 | B1 | C1 | D1 | E1 | F1 | G1 | H1 => 7,
        }
    }

    fn col(self) -> usize {
        use Pos::*;

        match self {
            A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8 => 0,
            B1 | B2 | B3 | B4 | B5 | B6 | B7 | B8 => 0,
            C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 => 0,
            D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 => 0,
            E1 | E2 | E3 | E4 | E5 | E6 | E7 | E8 => 0,
            F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 => 0,
            G1 | G2 | G3 | G4 | G5 | G6 | G7 | G8 => 0,
            H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 => 0,
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
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
    fn new() -> Self {
        let (rows, cols) = (8, 8);
        Self {
            squares: vec![vec![None; cols]; rows],
        }
    }

    fn dimensions(&self) -> (usize, usize) {
        (self.squares.len(), self.squares[0].len())
    }

    fn from_fen(fen: String) -> Result<Self, ParseError> {
        let mut board = Board::new();
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

    fn fen(&self) -> String {
        let (rows, _) = self.dimensions();
        let mut fen: Vec<String> = Vec::new();

        for row in self.squares.iter() {
            for cell in row {
                if let Some(sq) = cell {
                    fen.push(sq.fen());
                    continue;
                }
                if let Some(last) = fen.last_mut() {
                    if let Ok(num) = last.parse::<usize>() {
                        *last = (num + 1).to_string();
                        continue;
                    }
                }
                fen.push("1".to_string());
            }
            fen.push("/".to_string());
        }

        fen.pop();

        fen.join("")
    }

    fn get(&self, pos: Pos) -> Option<Square> {
        let (row, col) = pos.into();
        self.squares[row][col]
    }

    fn set(&mut self, pos: Pos, sq: Option<Square>) {
        let (row, col) = pos.into();
        self.squares[row][col] = sq;
    }
}

impl Default for Board {
    fn default() -> Self {
        let default_placement = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR";

        Self::from_fen(default_placement.to_string()).unwrap()
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

struct HistoryItem {
    mov: Move,
    fen: String,
}

impl HistoryItem {
    fn new(mov: Move, fen: String) -> Self {
        Self {
            mov,
            fen
        }
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
    history: Vec<HistoryItem>,
}

impl Default for Game {
    fn default() -> Self {
        Self {
            board: Board::default(),
            turn: Color::White,
            castling: Castling::default(),
            ep_square: None,
            history: Vec::new(),
        }
    }
}

impl Game {
    fn moves_unchecked(&self, pos: Pos) -> Vec<Move> {

        if let Some(sq) = self.board.get(pos) {
            let mut offsets = Vec::new();
            let mut should_promote = false;

            match sq.piece {
                Piece::King | Piece::Knight => offsets.extend(sq.piece.offsets().into_iter()),
                Piece::Bishop | Piece::Rook | Piece::Queen => {
                    let (r, c) = (pos.row() as i32, pos.col() as i32);

                    for (dr, dc) in sq.piece.offsets().into_iter() {
                        for i in 1.. {
                            match Pos::try_from((r + dr * i, c + dc * i)) {
                                Ok(pos) => {
                                    offsets.push((r + dr * i, c + dc * i));
                                    if self.board.get(pos).is_some() {
                                        break;
                                    }
                                }
                                Err(_) => {
                                    break;
                                }
                            }
                        }
                    }
                }
                Piece::Pawn => {
                    let mut pawn_offsets = sq.piece.offsets();
                    if sq.color == Color::White {
                        pawn_offsets.iter_mut().for_each(|(row, col)| { *row *= -1; *col *= -1; })
                    }
                    offsets.push(pawn_offsets[0]);
                    if pos.rank(sq.color) == 2 {
                        offsets.push(pawn_offsets[1]);
                    } else if pos.rank(sq.color) == 7 {
                        should_promote = true;
                    }
                    if let Some(ep) = self.ep_square {
                        todo!("finish pawn ep");
                    }

                }
            }

            if should_promote {
                return [Piece::Bishop, Piece::Knight, Piece::Rook, Piece::Queen].into_iter().map(|piece| Move::new(pos, pos.with_offsets(&offsets)[0]).promote(piece)).collect();
            } else {
                return pos.with_offsets(&offsets).into_iter().map(|to| Move::new(pos, to)).collect();
            }
        }

        vec![]
    }

    fn make_move(&mut self, mov: Move) -> Result<(), IllegalMoveError> {
        if !self.moves_unchecked(mov.from).contains(&mov) {
            return Err(IllegalMoveError);
        }

        let from = self.board.get(mov.from).ok_or(IllegalMoveError)?;

        // Deny moving enemy pieces
        if from.color != self.turn {
            return Err(IllegalMoveError);
        }

        self.history.push(HistoryItem::new(mov, self.board.fen()));

        self.board.set(mov.to, Some(from));
        self.board.set(mov.from, None);

        self.turn = !self.turn;

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

    let mov = Move::new(Pos::A1, Pos::A3).promote(Piece::Queen);

    // game.make_move(mov).unwrap();

    println!("{:#?}", game.board);
}
