toBinary :: Int -> [Int] -> [Int]
toBinary 0 x = x
toBinary t x = toBinary (div t 2) ([mod t 2] ++ x)

makeZero :: Int -> String
makeZero 0 = ""
makeZero x = "0" ++ makeZero (x - 1)

binary2str :: [Int] -> String
binary2str [] = ""
binary2str (x : xs) = show x ++ binary2str xs

main :: IO ()
main = do
    n <- readLn :: IO Int
    let ns = toBinary n []
    putStrLn $ (makeZero $ 10 - (length ns)) ++ (binary2str ns)