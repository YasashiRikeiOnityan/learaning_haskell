toBinary :: Int -> String
toBinary 1 = "1"
toBinary n = toBinary (n `div` 2) ++ if even n then "0" else "1"

assignZero :: Int -> String -> String
assignZero 0 str = str
assignZero x str = "0" ++ assignZero (x - 1) str

main :: IO ()
main = do
    n <- readLn :: IO Int
    let binary = toBinary n
    putStrLn $ assignZero (10 - length binary) binary