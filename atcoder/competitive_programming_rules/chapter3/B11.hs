module B11 where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )
import Data.Maybe ( fromJust )
import Data.Array ( Array, listArray, (!) )
import Data.List ( sort )
import Control.Monad ( replicateM )

lowerBound :: Int -> Int -> Array Int Int -> Int -> Int
lowerBound l r arr x
    | x < arr ! l = l - 1
    | arr ! r < x = r
    | otherwise = if x <= arr ! mid then lowerBound l (mid - 1) arr x else lowerBound (mid + 1) r arr x
        where mid = (l + r) `div` 2
 
main :: IO ()
main = do
    n <- readLn
    as <- map readInt . BS.words <$> BS.getLine
    q <- readLn
    xs <- concatMap (map readInt . BS.words) <$> replicateM q BS.getLine
    let arr = listArray (1, n) (sort as)
    mapM_ (BS.putStrLn . showInt . lowerBound 1 n arr) xs

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt

showInt :: Int -> ByteString
showInt = BS.pack . show