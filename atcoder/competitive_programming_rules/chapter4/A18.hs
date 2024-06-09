module A18 where

judge :: Int -> [Int] -> [Int] -> Bool
judge _ [] _ = False
judge s (a:as) bs = (s `elem` bs') || judge s as bs'
    where bs' = partialMap a bs
          partialMap :: Int -> [Int] -> [Int]
          partialMap _ [] = []
          partialMap x (y:ys)
            | x + y > s = y : partialMap x ys
            | otherwise = y : (x + y) : partialMap x ys

a18 :: Int -> [Int] -> String
a18 s as = if judge s as [0] then "Yes" else "No"

main :: IO ()
main = do
    [_, s] <- map read . words <$> getLine
    as <- map read . words <$> getLine
    putStrLn $ a18 s as
