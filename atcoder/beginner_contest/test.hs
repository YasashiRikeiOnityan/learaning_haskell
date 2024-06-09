import Data.List (unfoldr, transpose)

-- 文字列を指定した長さで分割する関数
chunk :: Int -> String -> [String]
chunk n = unfoldr (\str -> if null str then Nothing else Just (splitAt n str))

-- リストを指定した長さで分割する関数
chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr (\xs -> if null xs then Nothing else Just (splitAt n xs))

-- 指定したフォーマットで出力する関数
outputFormatted :: [String] -> IO ()
outputFormatted chunks = mapM_ putStrLn $ transpose chunks

main :: IO ()
main = do
    let str = "####.####"
    -- 長さ3^(k+1)の文字列と仮定して、3^k文字ずつ分割
    let k = 3  -- 例えば、k = 3の場合（3^4 = 81 文字）
    let n = 3
    let chunks = chunk n str
    -- 各チャンクを先頭から順に出力
    outputFormatted chunks
