module ABC356D where

import Data.Bits ( testBit )

modulo :: Integer
modulo = 998244353

sumPopCount :: Integer -> Integer -> Integer
sumPopCount n m = sumBits 0 0
  where
    highestBit = floor (logBase 2 (fromIntegral n)) + 1
    sumBits :: Integer -> Integer -> Integer
    sumBits acc bit
      | bit >= highestBit = acc `mod` modulo
      | otherwise =
          let bitCount = (n + 1) `div` (2 ^ (bit + 1)) * (2 ^ bit) + max 0 ((n + 1) `mod` (2 ^ (bit + 1)) - (2 ^ bit))
              bitPopCount = if testBit m (fromIntegral bit) then bitCount else 0
          in sumBits ((acc + bitPopCount) `mod` modulo) (bit + 1)

main :: IO ()
main = do
    [n, m] <- map read . words <$> getLine :: IO [Integer]
    print (sumPopCount n m)
