module ABC043B where

main :: IO ()
main = do
    str <- getLine
    putStrLn . snd $ editor (str, [])

editor :: (String, String) -> (String, String)
editor ([], str) = ([], str)
editor (x : xs, str)
    | x == '0' = editor (xs, str ++ "0")
    | x == '1' = editor (xs, str ++ "1")
    | x == 'B' = case str of
        [] -> editor (xs, str)
        _ -> editor (xs, init str)
    | otherwise = editor (xs, str)
