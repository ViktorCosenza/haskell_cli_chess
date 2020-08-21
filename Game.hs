module Game where
import Data.Matrix(matrix, (!), setElem)

import Types
import Input
import Output
import Utils

emptyCell :: (Int, Int) -> ChessBoard -> Bool
emptyCell = isEmpty Space

findFirst :: [(Int, Int)] -> ChessBoard -> ChessPiece
findFirst ps board = first 
    where 
        first   = (((!) board) . snd . head . reverse) (zip (mapUntilTaken ps) ps) 
        mapOccupied = map (\pos -> emptyCell pos board)
        mapUntilTaken ps = takeWhileWithFirst id (mapOccupied ps)

checkAttackerIn :: [Piece] -> Player -> [(Int, Int)] -> ChessBoard -> Bool
checkAttackerIn attackers pl ps board = elem (piece first) attackers && pl /= player first
    where first = findFirst ps board 

checkLine :: Player -> [(Int, Int)] -> ChessBoard -> Bool
checkLine = checkAttackerIn [Queen, Rook]

checkDiag :: Player -> [(Int, Int)] -> ChessBoard -> Bool
checkDiag = checkAttackerIn [Queen, Bishop]

checkPawn :: Player -> (Int, Int) -> ChessBoard -> Bool
checkPawn p (x, y) board = length pawns /= 0
    where 
        pawns = filter (\pc -> piece pc == Pawn) attackers
        attackers = filter (\piece -> player piece /= p) pieces
        pieces = map (\position -> board ! position) positions 
        positions = (filter isValidPos . map (\offX -> (offX, yOffset))) [x + 1, x - 1]
        yOffset = if p == White then 1 else -1

checkKing :: Player -> (Int, Int) -> ChessBoard -> Bool
checkKing p (x, y) board = length king /= 0
    where 
        king = filter (\pc -> piece pc == King) attackers
        attackers = filter (\piece -> player piece /= p) pieces
        pieces = map (\position -> board ! position) positions 
        positions = (filter isValidPos . map (\(offX, offY) -> (x + offX, y + offY))) offsets
        offsets = foldl (++) [] $ map (\off1 -> (map (\off2 -> (off1, off2)) [-1..1])) [-1..1]

checkKnight :: Player -> (Int, Int) -> ChessBoard -> Bool
checkKnight p (x, y) board = length knights /= 0
    where
        knights = filter (\pc -> piece pc == Knight) attackers
        attackers = filter (\piece -> player piece /= p) pieces
        pieces = map (\position -> board ! position) positions
        positions = (filter isValidPos . map (\(offX, offY) -> (x + offX, y + offY))) offsets ++ reverse offsets 
        offsets = [(2, 1), (1, 2), (-2, 1), (2, -1), (-2, -1)]

verticalLine :: Int -> [Int] -> [(Int, Int)]
verticalLine x = map (\y -> (x, y))

horizontalLine :: Int -> [Int] -> [(Int, Int)]
horizontalLine y = map (\x -> (x, y)) 

stepLine :: Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
stepLine length (x, y) (stepX, stepY) = map getStep [1..length]
    where getStep currStep = (x + currStep * stepX, y + currStep * stepY) 

checkVerticalAttack :: Player -> (Int, Int) -> ChessBoard -> Bool
checkVerticalAttack p (x, y) board = 
    (checkLine p v1 board) || 
    (checkLine p v2 board) ||
    (checkLine p h1 board) ||
    (checkLine p h2 board) 
        where
            v1 = verticalLine x [1..y - 1]
            v2 = verticalLine x [y + 1..8]
            h1 = horizontalLine y [1..x -1]
            h2 = horizontalLine y [x + 1..8]

checkDiagonalAttack :: Player -> (Int, Int) -> ChessBoard -> Bool
checkDiagonalAttack p (x, y) board = foldl (||) False attacks
        where
            attacks = map (\d -> checkDiag p d board) diags
            diags = map (stepLine 8 (x, y)) offsets
            offsets = [(1, 1), (1, -1), (-1, 1), (-1, -1)]

attackedCell :: Player -> (Int, Int) -> ChessBoard -> Bool
attackedCell pl pos board = 
    (checkVerticalAttack pl pos board) ||
    (checkDiagonalAttack pl pos board) ||
    (checkPawn pl pos board) ||
    (checkKing pl pos board)

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
        player = if x <= 2 then Black else White
        createPiece p = ChessPiece player p

rawMovePiece :: ChessMove -> ChessBoard -> ChessBoard
rawMovePiece ((x1, y1), (x2, y2)) m = (setElem Space p1   . setElem piece p2) m 
    where 
        piece = m ! p1
        p1 = (x1, y1)
        p2 = (x2, y2)

validPawnMove :: ChessMove -> ChessBoard -> Bool
validPawnMove ((x1, y1), (x2, y2)) board 
    | elem (x2, y2) diags && hasPiece (x2, y2) = True
    | x1 /= x2 = False
    | elem (x2, y2) forwards && not (hasPiece (x2, y2)) = False
    | otherwise = False
        where 
            hasPiece pos = emptyCell pos board 
            p = player (board ! (x1, y1))
            forwards = if p == White
                then [(x1, y1 + 1)] ++ if (x1 == 2 && (not . hasPiece) (x1, y1 + 1)) then [(x1, y1 + 2)] else []
                else [(x1, y1 - 1)] ++ if (x1 == 7 && (not . hasPiece) (x1, y1 - 1)) then [(x1, y1 - 2)] else []
            diags = if p == White 
                        then filter (\x -> hasPiece x && isValidPos x)[(x1 + 1, y1 + 1), (x1 - 1, y1 + 1)]
                        else filter (\x -> hasPiece x && isValidPos x)[(x1 + 1, y1 - 1), (x1 - 1, y1 - 1)]

validPieceMove :: Piece -> ChessMove -> ChessBoard -> Bool
--validPieceMove Pawn = validPawnMove
validPieceMove _ = \m b -> True

validChessMove :: Player -> ChessMove -> ChessBoard -> Bool
validChessMove p (from, to) board
    | chesspiece == Space = False
    | p /= player chesspiece = False
    | otherwise = validPieceMove rawPiece (from, to) board
    where 
        chesspiece = board ! from
        rawPiece = piece chesspiece 

gameTick :: Player -> ChessBoard -> String -> (Player, ChessBoard)
gameTick p board moveStr
    | not (validChessMove p move board) = (p, board)
    | otherwise = (nextPlayer p, rawMovePiece move board)
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
    if not isValidMove then gameLoop player board
    else let (nextPlayer, nextBoard) = gameTick player board moveStr
        in if nextPlayer == player 
            then do
                putStrLn $ "Invalid move! Try again..."
                gameLoop nextPlayer nextBoard
            else do 
                putStr $ "Moved"
                gameLoop nextPlayer nextBoard