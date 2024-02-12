import qualified Data.ByteString.Char8 as BS
import Data.Maybe ( fromJust )
import Control.Monad ( replicateM )
import Data.Char ( isSpace )
import Data.List ( unfoldr )

readInt :: IO Int
readInt = fst . fromJust . BS.readInt <$> BS.getLine

toIntList :: BS.ByteString -> [Int]
toIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)

readIntList :: IO [Int]
readIntList = toIntList <$> BS.getLine

readIntLists :: Int -> IO [[Int]]
readIntLists n = replicateM n readIntList

abc330a :: Int -> [Int] -> Int
abc330a l as = length [x | x <- as, l <= x]

main :: IO ()
main = do
    [n, l] <- readIntList :: IO [Int]
    as <- readIntList :: IO [Int]
    print $ abc330a l as