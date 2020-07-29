{-# LANGUAGE OverloadedStrings #-}

module Output where
import Rainbow(fore, magenta, green, Chunk, putChunksLn)
import Data.Matrix(toList, mapPos)
import Data.Function ((&))

import Utils
import Types

pieceStr p
    | p == Pawn   = "p"
    | p == Rook   = "R"
    | p == Knight = "N"
    | p == Bishop = "B"
    | p == Queen  = "Q"
    | p == King   = "K"

printChessboard :: ChessBoard -> IO ()
printChessboard = putChunksLn . getChessboardStr

getChessboardStr :: ChessBoard -> [Chunk]
getChessboardStr c = 
    "   A B C D E F G H \n":
    (insertEvery 8 (reverse ["1 ", "2 ", "3 ", "4 ", "5 ", "6 ", "7 ", "8 "]) . toList . mapPos elemToChunk) c

elemToChunk :: (Eq a1, Num a1) => (a2, a1) -> ChessPiece -> Chunk
elemToChunk (r, c) e = if c == 8 then chunk <> "\n" else chunk
        where 
            chunk = chunkPiece e

chunkPiece :: ChessPiece -> Chunk
chunkPiece Space                    = " ."
chunkPiece (ChessPiece White piece) = " " <> pieceStr piece & fore magenta--(blue <> only256 blue)
chunkPiece (ChessPiece Black piece) = " " <> pieceStr piece & fore green--(Rainbow.red <> Rainbow.only256 Rainbow.red)
