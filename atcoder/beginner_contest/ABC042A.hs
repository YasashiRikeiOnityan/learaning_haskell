module Main where

import Data.List ( sort )

solve :: [Int] -> String
solve xs = if sort xs == [5, 5, 7] then "Yes" else "No" 

main :: IO ()
main = do
    xs <- map read . words <$> getLine
    putStrLn $ solve xs