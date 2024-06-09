module RectangleCutting where

main :: IO ()
main = do
    [w, h, x, y] <- map read . words <$> getLine :: IO [Double]
    putStrLn $ show ((w * h) / 2) ++ " " ++ if w / 2 == x && h / 2 == y then "1" else "0"