import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Control.Monad
import Data.Char
import Data.List

readInt :: IO Int
readInt = fst . fromJust . BS.readInt <$> BS.getLine

toIntList :: BS.ByteString -> [Int]
toIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)

readIntList :: IO [Int]
readIntList = toIntList <$> BS.getLine

readIntLists :: Int -> IO [[Int]]
readIntLists n = replicateM n readIntList

main :: IO ()
main = do
    [_, l, r] <- readIntList :: IO [Int]
    as <- readIntList :: IO [Int]
    let ans = abc330b l r as
    putStrLn $ foldr ($) "" $ intersperse (' ' :) $ map shows ans

abc330b :: Int -> Int -> [Int] -> [Int]
abc330b l r = map (max l . min r)