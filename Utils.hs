module Utils where
import Data.Matrix(Matrix, mapPos)

setMatrixElem :: a -> (Int, Int) -> Matrix a -> Matrix a
setMatrixElem a pos = mapPos f
    where f p
            | p == pos = const a
            | otherwise = id


insertEvery :: Int -> [a] -> [a] -> [a]
insertEvery 0 _ as = as
insertEvery n es as 
    | null es       = as
    | length as < n = head es : as
    | otherwise     = head es : take n as ++ insertEvery n (tail es) (drop n as)