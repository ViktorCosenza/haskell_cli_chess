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
} | Space deriving(Show, Eq)

pieceStr p
    | p == Pawn   = "p"
    | p == Rook   = "R"
    | p == Knight = "N"
    | p == Bishop = "B"
    | p == Queen  = "Q"
    | p == King   = "K"


chunkPiece Space                    = "."
chunkPiece (ChessPiece White piece) = pieceStr piece & Rainbow.fore Rainbow.magenta--(Rainbow.blue <> Rainbow.only256 Rainbow.blue)
chunkPiece (ChessPiece Black piece) = pieceStr piece & Rainbow.fore Rainbow.green--(Rainbow.red <> Rainbow.only256 Rainbow.red)

elemToChunk (r, c) e = if c == 8 then chunk <> "\n" else chunk
        where 
            chunk = chunkPiece e

getChessboardStr = Matrix.toList . Matrix.mapPos elemToChunk

initialPosition :: (Int, Int) -> ChessPiece
initialPosition (x, y) 
    | x == 2 || x == 7 = createPiece Pawn
    | x /= 1 && x /= 8 = Space
    | y == 1 || y == 8 = createPiece Rook
    | y == 2 || y == 7 = createPiece Knight
    | y == 3 || y == 6 = createPiece Bishop
    | y == 4           = createPiece Queen
    | y == 5           = createPiece King
    | otherwise        = Space
    where 
        player = if x <= 2 then White else Black 
        createPiece p = ChessPiece player p

main = Rainbow.putChunksLn $ getChessboardStr (Matrix.matrix 8 8 initialPosition)