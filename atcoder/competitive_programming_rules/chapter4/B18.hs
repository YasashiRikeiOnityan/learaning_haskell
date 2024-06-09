module B18 where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )
import Data.Maybe ( fromJust )
import Data.Bifunctor ( bimap )

judge :: Int -> [(Int, Int)] -> [(Int, [Int])] -> [(Int, [Int])]
judge _ [] bs = bs
judge s (a:as) bs = judge s as (partialMap s a bs)

partialMap :: Int -> (Int, Int) -> [(Int, [Int])] -> [(Int, [Int])]
partialMap _ _ [] = []
partialMap s x (y:ys)
    | snd x + fst y > s = y : partialMap s x ys
    | snd x + fst y == s = [(s, fst x : snd y)]
    | otherwise = y : bimap (snd x +) (fst x :) y : partialMap s x ys

b18 :: Int -> [(Int, Int)] -> String
b18 s as = if fst res == s then (show . length . snd $ res) ++ "\n" ++ (unwords . map show . reverse $ snd res) else "-1"
    where res = last $ judge s as [(0, [])]

main :: IO ()
main = do
    [_, s] <- map readInt . BS.words <$> BS.getLine
    as <- map readInt . BS.words <$> BS.getLine
    print . length $ judge s (zip [1..] as) [(0, [])]
    --putStrLn $ b18 s (zip [1..] as)

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt
