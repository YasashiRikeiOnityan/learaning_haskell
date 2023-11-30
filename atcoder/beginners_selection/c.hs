main :: IO ()
main = do
    ss <- getLine :: IO String
    let ans = length $ filter (== "1") (map (: []) ss)
    print ans