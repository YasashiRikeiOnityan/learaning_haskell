module B12 where

f :: Double -> Double
f x = x ** 3 + x

binarySearch :: Double -> Double -> Double -> Double
binarySearch l r n
    | abs (f mid - n) <= 0.001 = mid
    | f mid < n = binarySearch mid r n
    | otherwise = binarySearch l mid n
    where
        mid = (l + r) / 2

main :: IO ()
main = do
    n <- readLn
    print $ binarySearch 1 47 n