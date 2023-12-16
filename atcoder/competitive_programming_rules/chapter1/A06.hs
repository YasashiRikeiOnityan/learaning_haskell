import Control.Monad ( replicateM )

cumulativeSum :: [Int] -> [Int]
cumulativeSum = scanl (+) 0

calc :: [Int] -> [Int] -> Int
calc s [l, r] = (s !! r) - (s !! (l - 1))

makeOutput :: [Int] -> String
makeOutput [] = ""
makeOutput (x : xs) = show x ++ "\n" ++ makeOutput xs

main :: IO ()
main = do
    [_, q] <- map read . words <$> getLine :: IO [Int]
    as <- map read . words <$> getLine :: IO [Int]
    lr <- replicateM q getLine :: IO [String]
    let lr_ = map words lr
    let lrs = map (\m -> map read m :: [Int]) lr_
    putStrLn $ makeOutput $ map (calc (cumulativeSum as)) lrs