module Input where
import Data.Char(ord, toLower)

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

letterToPos :: Char -> Int
letterToPos = (+)(-96) . ord . toLower

validChessMovestring :: String -> Bool
validChessMovestring (l1:x1:l2:x2:[]) = (l1 /= l2 || x1 /=x2) && foldl (&&) True ((map charf (l1:l2:"")) ++ (map numberf (x1:x2:"")))
    where 
            charf = filterChar allowedLetters
            numberf = filterChar allowedNumbers
validChessMovestring _ = False

parseInput :: IO ((Bool, [Char]))
parseInput = do
    line <- getLine
    let lower = map toLower line
    let noSpace = filter (/= ' ') lower 
    return (validChessMovestring noSpace, noSpace)

toChessMove :: String -> ((Int, Int), (Int, Int))
toChessMove (l1:x1:l2:x2:[]) = (p1, p2)
    where 
        p1 = (9 - ord x1, letterToPos l1)
        p2 = (9 - ord x2, letterToPos l2)
toChessMove _ = ((-1, -1), (-1, -1))