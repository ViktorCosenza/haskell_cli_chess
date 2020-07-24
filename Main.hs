{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Matrix as Matrix
import qualified Rainbow
import Data.Function ((&))
import Data.Char(ord, toLower)
import System.IO

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

setMatrixElem :: a -> (Int, Int) -> Matrix.Matrix a -> Matrix.Matrix a
setMatrixElem a pos = Matrix.mapPos f
    where f p
            | p == pos = const a
            | otherwise = id



rawMovePiece :: (Int, Int) -> (Int, Int) -> Matrix.Matrix ChessPiece -> Matrix.Matrix ChessPiece
rawMovePiece (x1, y1) (x2, y2) m = (setMatrixElem Space p1  . setMatrixElem from p2) m 
    where 
        from = m Matrix.! p1
        p1 = (x1, y1)
        p2 = (x2, y2)

letterToPos :: Char -> Int
letterToPos = (+)(-96) . ord . toLower

movePiece :: (Char, Int) -> (Char, Int) -> Matrix.Matrix ChessPiece -> Matrix.Matrix ChessPiece
movePiece (l1, x1) (l2, x2) = rawMovePiece p1 p2
    where 
        p1 = (9 - x1, letterToPos l1)
        p2 = (9 - x2, letterToPos l2)
        

chunkPiece Space                    = " ."
chunkPiece (ChessPiece White piece) = " " <> pieceStr piece & Rainbow.fore Rainbow.magenta--(Rainbow.blue <> Rainbow.only256 Rainbow.blue)
chunkPiece (ChessPiece Black piece) = " " <> pieceStr piece & Rainbow.fore Rainbow.green--(Rainbow.red <> Rainbow.only256 Rainbow.red)

elemToChunk (r, c) e = if c == 8 then chunk <> "\n" else chunk
        where 
            chunk = chunkPiece e

insertEvery :: Int -> [a] -> [a] -> [a]
insertEvery 0 _ as = as
insertEvery n es as 
    | null es       = as
    | length as < n = head es : as
    | otherwise     = head es : take n as ++ insertEvery n (tail es) (drop n as)
            
            


getChessboardStr :: Matrix.Matrix ChessPiece -> [Rainbow.Chunk]
getChessboardStr c = 
    "   A B C D E F G H \n":
    (insertEvery 8 (reverse ["1 ", "2 ", "3 ", "4 ", "5 ", "6 ", "7 ", "8 "]) . Matrix.toList . Matrix.mapPos elemToChunk) c

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

initialChessboard :: Matrix.Matrix ChessPiece
initialChessboard = (Matrix.matrix 8 8 initialPosition)

printChessboard = Rainbow.putChunksLn . getChessboardStr

allowedLetters :: String
allowedLetters = "abcdefgh"

allowedNumbers :: String
allowedNumbers = "12345678"

allowedSymbols :: String
allowedSymbols = ",()"

allowedCharacters :: String
allowedCharacters = allowedLetters ++ allowedNumbers ++ allowedSymbols

filterChar :: [Char] -> Char -> Bool
filterChar fs c = let bs = map (== c) fs in foldr (||) False bs

filterString :: [Char] -> String -> String
filterString fs = (filter (filterChar fs)) . (map toLower)

validChessMovestring :: String -> Bool
validChessMovestring (l1:x1:l2:x2:[]) = foldl (&&) True ((map charf (l1:l2:"")) ++ (map numberf (x1:x2:"")))
    where 
            charf = filterChar allowedCharacters
            numberf = filterChar allowedNumbers
validChessMovestring _ = False

main = do 
    line <- getLine
    putStrLn line
    printChessboard initialChessboard