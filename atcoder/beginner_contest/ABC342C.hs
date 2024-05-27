module ABC342C where

import Control.Monad ( replicateM )
import Data.Array ( elems, Array, listArray, (!), (//) )
import Data.Maybe ( fromJust )
import Data.List ( elemIndex )

toPair :: [a] -> [(a, a)]
toPair [x, _, y] = [(x, y)]
toPair _ = []

myMap :: Array Int (Char, Char) -> (Char, Char) -> Array Int (Char, Char)
myMap arr (x, y) = arr // [idx, (x, y)]
    where idx = fromJust (elemIndex (x, y) (elems arr))

main :: IO ()
main = do
    n <- readLn :: IO Int
    str <- getLine
    q <- readLn :: IO Int
    cds <- concatMap toPair  <$> replicateM q getLine
    print cds