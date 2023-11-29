import Control.Monad ( replicateM )

cumulativeSum :: [Int] -> [Int]
cumulativeSum [] = []
cumulativeSum [x] = [x]
cumulativeSum (x : xs) = x : cumulativeSum ((x + head xs) : tail xs)

calc :: [Int] -> [Int] -> Int
calc s [l, r] = if l == 1 then s !! r else (s !! (r - 1)) - (s !! (l - 2))

makeOutput :: [Int] -> String
makeOutput [x] = show x
makeOutput (x : xs) = show x ++ "\n" ++ makeOutput xs

main :: IO ()
main = do
    [_, q] <- map read . words <$> getLine :: IO [Int]
    as <- map read . words <$> getLine :: IO [Int]
    lr <- replicateM q getLine :: IO [String]
    let x = map words lr
    let y = map (\m -> map read m :: [Int]) x
    putStrLn $ makeOutput $ map (calc (cumulativeSum as)) y