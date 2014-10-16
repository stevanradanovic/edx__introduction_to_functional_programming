module Main where

import Lecture

main :: IO ()
main = do
    putStrLn (show (quadruple 10))
    putStrLn (show (take (double 2) [1, 2, 3, 4, 5, 6]))
    putStrLn (show (factorial 10))
    putStrLn (show (average [1, 2, 3, 4, 5]))