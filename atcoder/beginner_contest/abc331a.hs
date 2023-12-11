main :: IO ()
main = do
    [_M, _D] <- map read . words <$> getLine :: IO [Int]
    [y, m, d] <- map read . words <$> getLine :: IO [Int]
    putStrLn $ list2String (tomorrow [_M, _D] [y, m, d])
    
tomorrow :: [Int] -> [Int] -> [Int]
tomorrow [_M, _D] [y, m, d]
    | _M == m && _D == d = [y + 1, 1, 1]
    | _D == d            = [y, m + 1, 1]
    | otherwise          = [y, m, d + 1]
tomorrow _ _ = []
    
list2String :: Show a => [a] -> String
list2String []       = ""
list2String [x]      = show x
list2String (x : xs) = show x ++ " " ++ list2String xs