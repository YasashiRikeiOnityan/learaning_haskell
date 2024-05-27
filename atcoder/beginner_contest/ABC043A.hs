module ABC043A where

main :: IO ()
main = do
    n <- readLn :: IO Int
    print $ candy n

candy :: Int -> Int
candy 1 = 1
candy n = candy (n - 1) + n
