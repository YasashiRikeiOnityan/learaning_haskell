module Thin where

import Control.Monad ( replicateM )

printTwice :: String -> IO ()
printTwice x = do 
    putStrLn x
    putStrLn x

main :: IO ()
main = do
    [h, _] <- map read . words <$> getLine
    css <- replicateM h getLine
    mapM_ printTwice css
    