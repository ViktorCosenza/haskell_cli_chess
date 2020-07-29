module Game where

import Data.Matrix(matrix, (!))

import Types
import Input
import Output
import Utils

initialChessboard :: ChessBoard
initialChessboard = (matrix 8 8 initialPosition)

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


rawMovePiece :: ChessMove -> ChessBoard -> ChessBoard
rawMovePiece ((x1, y1), (x2, y2)) m = (setMatrixElem Space p1  . setMatrixElem from p2) m 
    where 
        from = m ! p1
        p1 = (x1, y1)
        p2 = (x2, y2)

validChessMove :: String -> Bool
validChessMove _ = True

gameTick :: Player -> ChessBoard -> String -> (Player, ChessBoard)
gameTick player board moveStr
    | not (validChessMove moveStr) = (player, board)
    | otherwise = (nextPlayer player, board)
        where 
            move = toChessMove (moveStr)
            nextPlayer White = Black
            nextPlayer Black = White

gameLoop :: Player -> ChessBoard -> IO() 
gameLoop player board = do
    printChessboard board
    putStrLn $ show player ++ " to move:"

    {-- Parse user input and check--}
    (isValidMove, moveStr) <- parseInput

    putStrLn $ "Valid: " ++ show isValidMove
    putStrLn $ "Move: " ++ show moveStr
    {-- Next move --}
    let (nextPlayer, nextBoard) = gameTick player board moveStr
    gameLoop nextPlayer nextBoard