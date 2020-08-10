module Utils where
import Data.Matrix(Matrix, mapPos, (!))

isValidPos :: (Int, Int) -> Bool
isValidPos (x, y) = x > 0 && x <= 8 && y > 0 && y <= 8 

at :: Int -> [a] -> a
at i = head . drop (i - 1)

takeWhileWithFirst :: (a -> Bool) -> [a] -> [a]
takeWhileWithFirst f as 
    | length heads == length as = heads
    | otherwise = heads ++ [at (1 + length heads) as]
    where heads = takeWhile f as

setMatrixElem :: a -> (Int, Int) -> Matrix a -> Matrix a
setMatrixElem a pos = mapPos f
    where f p
            | p == pos = const a
            | otherwise = id

isEmpty :: Eq a => a -> (Int, Int) -> Matrix a -> Bool
isEmpty empty pos m 
    | m ! pos == empty = True
    | otherwise = False

insertEvery :: Int -> [a] -> [a] -> [a]
insertEvery 0 _ as = as
insertEvery n es as 
    | null es       = as
    | length as < n = head es : as
    | otherwise     = head es : take n as ++ insertEvery n (tail es) (drop n as)