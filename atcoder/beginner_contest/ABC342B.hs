module ABC342B where

import Control.Monad ( replicateM )
import Data.Array ( elems, Array, listArray )
import Data.Maybe ( fromJust )
import Data.List ( elemIndex )

toPair :: [Int] -> [(Int, Int)]
toPair [x, y] = [(x, y)]
toPair _ = []

solve :: Array Int Int -> (Int, Int) -> Int
solve prr (x, y) = if posX < posY then x else y
    where posX = fromJust $ elemIndex x (elems prr)
          posY = fromJust $ elemIndex y (elems prr)

main :: IO ()
main = do
    n <- readLn :: IO Int
    ps <- map read . words <$> getLine :: IO [Int]
    q <- readLn :: IO Int
    abs <- concatMap (toPair . map read . words) <$> replicateM q getLine
    mapM_ (print . solve (listArray (1, n) ps)) abs