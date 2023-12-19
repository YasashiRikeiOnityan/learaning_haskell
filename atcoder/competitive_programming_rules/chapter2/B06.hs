import Control.Monad ( replicateM )

-- cumulativeSum :: Int -> [Int] -> [Int] -> [Int]
-- cumulativeSum t i [] = if length i > 1 then tail i else []
-- cumulativeSum t i [x] = if t == x then tail $ i ++ [1 + last i] else tail $ i ++ [last i]
-- cumulativeSum t i (x : xs) = if t == x then cumulativeSum t (i ++ [1 + last i]) xs else cumulativeSum t (i ++ [last i]) xs

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
minus (x : xs) (y : ys) =(x - y) : minus xs ys

makeOutput :: [Int] -> String
makeOutput x = unlines $ map (\x -> if x > 0 then "win" else if x == 0 then "draw" else "lose") x

main :: IO ()
main = do
    n <- readLn :: IO Int
    as <- map read . words <$> getLine :: IO [Int]
    q <- readLn :: IO Int
    lr <- replicateM q getLine :: IO [String]
    let x = map words lr :: [[String]]
    let y = map (\m -> map read m :: [Int]) x :: [[Int]]
    let win = cumulativeSumOne as :: [Int]
    let lose = cumulativeSumZero as :: [Int]
    putStrLn $ makeOutput (minus (map (calc win) y) (map (calc lose) y)) :: IO ()