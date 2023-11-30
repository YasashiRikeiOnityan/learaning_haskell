{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad (replicateM_)
import Data.Array.Unboxed (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace)
import Data.List (scanl', unfoldr)

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

cumSum :: Int -> [Int] -> UArray Int Int
cumSum n as = listArray (1, n + 1) $ scanl' (+) 0 as

f :: Int -> Int
f 0 = - 1
f _ = 1

main :: IO ()
main = do
  [n] <- getInts
  as <- getInts

  let s = cumSum n $ map f as

  q <- readLn @Int
  replicateM_ q $ do
    [l, r] <- getInts

    putStrLn $
      let result = s ! succ r - s ! l
       in if
              | result > 0 -> "win"
              | result == 0 -> "draw"
              | otherwise -> "lose"


{-
import Control.Applicative
import Control.Monad

cumulative_sum :: Int -> [Int] -> [Int] -> [Int]
cumulative_sum t i [] = if length i > 1 then tail i else []
cumulative_sum t i [x] = if t == x then tail $ i ++ [1 + (last i)] else tail $ i ++ [last i]
cumulative_sum t i (x : xs) = if t == x then cumulative_sum t (i ++ [1 + (last i)]) xs else cumulative_sum t (i ++ [last i]) xs

cumulativeSumOne :: [Int] -> [Int]
cumulativeSumOne [] = []
cumulativeSumOne (x : xs) = scanl (+) x xs

cumulativeSumZero :: [Int] -> [Int]
cumulativeSumZero [] = []
cumulativeSumZero x = cumulativeSumOne $ map (\m -> if m == 0 then 1 else 0) x

calc :: [Int] -> [Int] -> Int
calc s [l, r] = if l == 1 then s !! (r - 1) else (s !! (r - 1)) - (s !! (l - 2))

minus :: [Int] -> [Int] -> [Int]
minus [] [y] = [-y]
minus [x] [] = [x]
minus [x] [y] = [x - y]
minus (x : xs) (y : ys) =[x - y] ++ (minus xs ys)

make_output :: [Int] -> String
make_output x = unlines $ map (\x -> if x > 0 then "win" else if x == 0 then "draw" else "lose") x

main :: IO ()
main = do
    n <- readLn :: IO Int
    as <- map read . words <$> getLine :: IO [Int]
    q <- readLn :: IO Int
    lr <- replicateM q getLine
    let x = map words lr
    let y = map (\m -> map read m :: [Int]) x
    let win = cumulativeSumOne as
    let lose = cumulativeSumZero as
    putStrLn $ make_output (minus (map (calc win) y) (map (calc lose) y))

import Control.Monad

cumulativeSum :: [Int] -> [Int]
cumulativeSum [] = []
cumulativeSum (x : xs) = scanl (+) x xs

count :: [Int] -> Int
count xs = 2 * (sum xs) - (length xs)

winner :: [Int] -> [Int] -> Int
winner s [l, r] = (count $ take (r-l+1) (drop (l-1) s))

process :: [Int] -> [[Int]] -> [Int]
process xs qs = map (winner s) qs
    where
        s = cumulativeSum xs

main :: IO ()
main = do
    n <- readLn :: IO Int
    as <- map read . words <$> getLine :: IO [Int]
    q <- readLn :: IO Int
    lr <- replicateM q (map read . words <$> getLine) :: IO [[Int]]
    let result = process as lr
    putStrLn $ unlines $ map (\x -> if x > 0 then "win" else if x == 0 then "draw" else "lose") result
-}