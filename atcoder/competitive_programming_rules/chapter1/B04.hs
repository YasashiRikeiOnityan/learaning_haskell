bin2list :: Int -> [Int] -> [Int]
bin2list 0 ys = ys
bin2list x ys = bin2list (x `div` 10) (x `mod` 10 : ys)

list2dec :: [Int] -> Int
list2dec [] = 0
list2dec [x] = x
list2dec (x : xs) = x * (2 ^ length xs) + list2dec xs

main :: IO ()
main = do
    n <- readLn :: IO Int
    print $ list2dec $ bin2list n []