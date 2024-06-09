module MergeSequances where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )
import Data.Maybe ( fromJust )
import Data.List ( sort )

solve :: [Int] -> [Int] -> Int -> ([Int], [Int])  -> ([Int], [Int])
solve [] [] _ (ias, ibs) = (ias, ibs)
solve (_:as) [] i (ias, ibs) = solve as [] (succ i) (i:ias, ibs)
solve [] (_:bs) i (ias, ibs) = solve [] bs (succ i) (ias, i:ibs)
solve aas@(a:as) bbs@(b:bs) i (ias, ibs)
    | a < b     = solve as bbs (succ i) (i:ias, ibs)
    | otherwise = solve aas bs (succ i) (ias, i:ibs)

main :: IO ()
main = do
    _ <- getLine
    as <- map readInt . BS.words <$> BS.getLine :: IO [Int]
    bs <- map readInt . BS.words <$> BS.getLine :: IO [Int]
    let ans = solve as bs 0 ([], [])
    BS.putStrLn . BS.unwords . map (showInt . succ) . reverse . fst $ ans
    BS.putStrLn . BS.unwords . map (showInt . succ) . reverse . snd $ ans

readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt

showInt :: Int -> ByteString
showInt = BS.pack . show