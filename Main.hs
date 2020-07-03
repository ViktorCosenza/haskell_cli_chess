{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Matrix as Matrix
import qualified Rainbow
import Data.Function ((&))

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
} | None deriving(Show, Eq)

pieceStr :: Piece -> String
pieceStr p
    | p == Pawn   = "p"
    | p == Rook   = "R"
    | p == Knight = "N"
    | p == Bishop = "B"
    | p == Queen  = "Q"
    | p == King   = "K"


chunkPiece :: ChessPiece -> Rainbow.Chunk
chunkPiece None                     = "."
chunkPiece (ChessPiece White piece) = Rainbow.fromText (pieceStr piece) <> Rainbow.fore Rainbow.blue
chunkPiece (ChessPiece Black piece) = Rainbow.fromText (pieceStr piece) <> Rainbow.fore Rainbow.red



initialPosition :: (Int, Int) -> ChessPiece
initialPosition (x, y) 
    | x == 2 || x == 7 = createPiece Pawn
    | x /= 1 && x /= 8 = None
    | y == 1 || y == 8 = createPiece Rook
    | y == 2 || y == 7 = createPiece Knight
    | y == 3 || y == 6 = createPiece Bishop
    | y == 4           = createPiece Queen
    | y == 5           = createPiece King
    | otherwise        = None
    where 
        player = if x <= 2 then White else Black 
        createPiece p = ChessPiece player p

main = do
    print $ Matrix.matrix 8 8 initialPosition
    Rainbow.putChunkLn $ chunkPiece (ChessPiece White Knight)