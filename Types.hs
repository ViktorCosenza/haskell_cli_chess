module Types where
import Data.Matrix(Matrix)

data Player = Black | White deriving (Show, Eq)
data Piece = Pawn 
    | Rook
    | Knight
    | Bishop
    | Queen
    | King deriving (Show, Eq)

data ChessPiece = ChessPiece {
    player :: Player,
    piece :: Piece
} | Space deriving(Show, Eq)

type ChessMove = ((Int, Int), (Int, Int))
type ChessBoard = Matrix ChessPiece