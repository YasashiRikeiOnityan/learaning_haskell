import Data.List.Split (splitOn)

main :: IO ()
main = getLine >>= putStrLn . last . splitOn "."
