module ABC340C where

import qualified Data.IntMap as IM

memorizedRec :: IM.IntMap Int -> Int -> (Int, IM.IntMap Int)
memorizedRec im 1 = (0, im)
memorizedRec im n =
  case IM.lookup n im of
    Just x  -> (x, im)
    Nothing -> (cost, im3)
  where
    (cost1, im1) = memorizedRec im  (n `div` 2)
    (cost2, im2) = memorizedRec im1 ((n + 1) `div` 2)
    cost = cost1 + cost2 + n
    im3 = IM.insert n cost im2

solve :: Int -> Int
solve = fst . memorizedRec IM.empty

main :: IO ()
main = readLn >>= print . solve

