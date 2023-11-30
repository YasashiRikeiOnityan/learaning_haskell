-- リストの最大値を返す関数
maximum' :: Ord a => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)

-- n個のxからなるリストを返す関数
replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n - 1) x

-- 前からn個の要素リストを返す関数
take' :: Int -> [a] -> [a]
take' n (x : xs)
    | n <= 0    = []
    | otherwise = x : take' (n - 1) xs

-- take'よりtake''の方がよい
-- 必要ないものはわざわざ読ませない方がよい
take'' :: Int -> [a] -> [a]
take'' n (x : xs)
    | n < 0  = []
take'' _ []  = []
take'' n (x : xs) = x : take' (n - 1) xs

-- 逆順のリストを返す関数
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

-- 受け取った要素の無限リストを生成する関数
repeat' :: a -> [a]
repeat' x = x : repeat' x

-- 二つのリストから同じインデックス同士の要素でタプルを作り，それらのリストを返す関数
-- 返り値のリストの長さは，引数のリストのうち長さが短い方と等しくなる
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

-- ある要素がリストに含まれているかの真偽値を返す関数
elem' :: Eq a => a -> [a] -> Bool
elem' e [] = False
elem' e (x : xs)
    | e == x    = True
    | otherwise = e `elem'` xs
