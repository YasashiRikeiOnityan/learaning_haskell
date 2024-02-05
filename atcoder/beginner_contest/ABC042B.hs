module ABC042B where

import Control.Monad ( replicateM )
import Data.List ( sort )

main :: IO ()
main = do
    [n, _] <- map read . words <$> getLine
    ss <- replicateM n getLine
    putStrLn . concat $ sort ss