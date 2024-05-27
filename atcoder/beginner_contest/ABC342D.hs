module ABC342D where

import Data.List (sort)

-- 与えられた整数リストから2つの数字を選んでその積が平方数となる組み合わせの数を求める関数
countSquarePairs :: [Int] -> Int
countSquarePairs xs = countPairs sortedRoots
    where
        roots = map (round . sqrt . fromIntegral) xs
        sortedRoots = sort roots

-- ソートされた平方根リストから積が平方数となる組み合わせの数を求める関数
countPairs :: [Int] -> Int
countPairs [] = 0
countPairs [_] = 0
countPairs (x : xs) = countPairs' x xs + countPairs xs
    where
        countPairs' _ [] = 0
        countPairs' root (y : ys)
            | isSquare (x * y) = 1 + countPairs' root ys
            | otherwise = countPairs' root ys

-- 整数が平方数かどうかを判定する関数
isSquare :: Int -> Bool
isSquare n
    | n < 0 = False
    | otherwise = let root = round (sqrt (fromIntegral n))
                  in root * root == n

main :: IO ()
main = do
    n <- readLn :: IO Int
    as <- map read. words <$> getLine :: IO [Int]
    print $ countSquarePairs as