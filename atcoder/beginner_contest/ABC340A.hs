module ABC340A where

main :: IO ()
main = do
    [a, b, d] <- map read . words <$> getLine
    putStrLn . unwords . map show $ [a, a + d .. b]