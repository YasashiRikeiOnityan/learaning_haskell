justNum :: Int -> [Int] -> Bool
justNum t (x : xs) = any (\m -> m + x == t) xs || justNum t xs
justNum _ _ = False

justThousand :: [Int] -> Bool
justThousand (x : xs) = justNum (1000 - x) xs || justThousand xs
justThousand _ = False

main :: IO ()
main = do
    n <- read <$> getLine :: IO Int
    as <- map read . words <$> getLine :: IO [Int]
    if justThousand as then putStrLn "Yes" else putStrLn "No"