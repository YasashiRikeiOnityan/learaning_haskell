module ABC342A where

import Data.Maybe ( fromJust )
import Data.List ( elemIndex )

makeTFs :: Char -> [Char] -> [Bool]
makeTFs x = map (x ==)

solve :: [Bool] -> Int
solve bs = if lenT == 1 then 1 else 1 + fromJust (elemIndex False bs)
    where lenT = length $ filter id bs

main :: IO ()
main = do
    (s : str) <- getLine :: IO [Char]
    print $ solve (makeTFs s (s : str))