int2list :: Int -> [Int] -> [Int]
int2list 0 t = t
int2list x t = int2list (div x 10) (mod x 10 : t)

list2int :: [Int] -> Int
list2int [] = 0
list2int (x : xs) = x * (2 ^ length xs) + list2int xs

main :: IO ()
main = do
    x <- readLn
    print $ list2int $ int2list x []