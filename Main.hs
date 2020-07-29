{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Matrix as Matrix
import qualified Rainbow
import Control.Monad(unless)
import Data.Char(ord, toLower)
import System.IO

import Types
import Input
import Output
import Game

endGame :: Player -> IO()
endGame player = putStrLn $ winnerMsg player
    where 
        winnerMsg White = "White won!"
        winnerMsg Black = "Black won!"


main :: IO ()
main = gameLoop White initialChessboard