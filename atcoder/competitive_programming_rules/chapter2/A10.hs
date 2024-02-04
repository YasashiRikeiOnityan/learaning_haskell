module A10 where

import Control.Monad ( replicateM )
import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )
import Data.Maybe ( fromJust )
import Data.Array ( Array, listArray, (!) )

cumulativeMax :: (Int, Int) -> [Int] -> Array Int Int
cumulativeMax bounds_ as = listArray bounds_ $ scanl1 max as

cumulativeMaxFromRight :: (Int, Int) -> [Int] -> Array Int Int
cumulativeMaxFromRight bounds_ xs = listArray bounds_ $ scanr max (last xs) (init xs)

solve :: Array Int Int -> Array Int Int -> [Int] -> Int
solve  cumMaxL cumMaxR [l, r] = max (cumMaxL ! (l - 1)) (cumMaxR ! (r + 1))
solve _ _ _ = -1

main :: IO ()
main = do
    [n] <- map readInt . BS.words <$> BS.getLine
    as <- map readInt . BS.words <$> BS.getLine
    [d] <- map readInt . BS.words <$> BS.getLine
    lrs <- map (map readInt . BS.words) <$> replicateM d BS.getLine
    let bounds_ = (1, n)
    let ps = cumulativeMax bounds_ as
    let qs = cumulativeMaxFromRight bounds_ as
    mapM_ (print . solve ps qs) lrs

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt

showInt :: Int -> ByteString
showInt = BS.pack . show