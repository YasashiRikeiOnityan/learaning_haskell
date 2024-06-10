module B18 where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )
import Data.Maybe ( fromJust )
import Data.Bifunctor ( bimap )

judge :: Int -> [(Int, Int)] -> [(Int, [Int])] -> (Int, [Int])
judge _ [] _ = (-1, [])
judge s (a : as) bs = if fst checked == s then checked else judge s as bs'
    where
        bs' = partialMap s a bs
        checked = check s bs'

f :: Int -> [(Int, Int)] -> [(Int, [Int])] -> [(Int, [Int])]
f _ [] bs = bs
f s (a : as) bs = f s as bs'
    where
        bs' = partialMap s a bs

partialMap :: Int -> (Int, Int) -> [(Int, [Int])] -> [(Int, [Int])]
partialMap _ _ [] = []
partialMap s x (y : ys)
    | snd x + fst y > s = y : partialMap s x ys
    | snd x + fst y == s = [(s, fst x : snd y)]
    | otherwise = y : bimap (snd x +) (fst x :) y : partialMap s x ys

check :: Int -> [(Int, [Int])] -> (Int, [Int])
check _ [] = (-1, [])
check s (y : ys)
    | fst y == s = y
    | otherwise = check s ys

b18 :: Int -> [(Int, Int)] -> String
b18 s as = if fst res == s then (show . length . snd $ res) ++ "\n" ++ (unwords . map show . reverse $ snd res) else "-1"
    where res = judge s as [(0, [])]

main :: IO ()
main = do
    [_, s] <- map readInt . BS.words <$> BS.getLine
    as <- map readInt . BS.words <$> BS.getLine
    putStrLn $ b18 s (zip [1..] as)

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt
