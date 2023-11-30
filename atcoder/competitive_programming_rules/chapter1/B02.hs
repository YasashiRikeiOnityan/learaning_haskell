import Control.Applicative

main :: IO ()
main = do
    [a, b] <- map read . words <$> getLine
    let x = [a..b]
    if elem 0 $ map (\m -> 100 `mod` m) x then putStrLn "Yes" else putStrLn "No"