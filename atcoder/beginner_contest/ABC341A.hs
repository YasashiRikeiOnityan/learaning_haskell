module ABC341A where

solve :: Int -> String
solve 1 = "101"
solve i = "10" ++ solve (i - 1)

main :: IO ()
main = do
    n <- readLn
    putStrLn $ solve n