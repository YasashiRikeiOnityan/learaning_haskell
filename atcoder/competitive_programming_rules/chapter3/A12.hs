module A12 where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )
import Data.Maybe ( fromJust )

main :: IO ()
main = do
    [_, k] <- map readInt . BS.words <$> BS.getLine
    as <- map readInt . BS.words <$> BS.getLine
    print $ solve 0 0 k as
    
readInt :: ByteString -> Int
readInt = fst . fromJust . BS.readInt

-- k秒目の操作を行う．
-- 現在の枚数(now)がプリントしたい枚数(n)を超えている場合は，
-- 前の秒数で全てプリントできていると判定する．
-- そうでない場合は，時刻(k)とプリントした枚数を加えてもう一度関数を呼び出す．
solve :: Int -> Int -> Int -> [Int] -> Int
solve k now n as = if now >= n then k - 1 else solve (k + 1) next n as
    where
        next = case k of
            0 -> 0
            _ -> now + foldl (\acc x -> if k `mod` x == 0 then acc + 1 else acc) 0 as