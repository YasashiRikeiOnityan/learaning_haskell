main :: IO ()
main = readLn >>= putStr . grid 

grid :: Integral b => b -> String
grid 0 = "#"
grid k = unlines $ concat [
        (\r -> r ++ r ++ r) <$> rs,
        (\r -> r ++ replicate (3 ^ pred k) '.' ++ r) <$> rs,
        (\r -> r ++ r ++ r) <$> rs
    ] where rs = lines . grid $ pred k
