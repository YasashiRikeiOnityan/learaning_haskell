module A19 where

import Control.Monad ( replicateM )

type Capacity = Int
type Value = Int
type Weight = Int
data Item = Item { weight :: Weight, value :: Value } deriving Show
data KSProblem = KSP { capacity :: Capacity, items :: [Item]}

knapsack :: KSProblem -> Value
knapsack (KSP _ []) = 0
knapsack (KSP c (i : is))
    | weight i <= c = max (knapsack (KSP c is))
                          (value i + knapsack (KSP (c - weight i) is))
    | otherwise = knapsack (KSP c is)

toItem :: [Int] -> Item
toItem [x, y] = Item x y
toItem _ = Item 0 0

main :: IO ()
main = do
    [n, w] <- map read . words <$> getLine :: IO [Int]
    wvs <- map (toItem . map read . words) <$> replicateM n getLine
    print 0

data DPProblem p sc d = DPProblem {
    initial :: p,
    isTrivial :: p -> Bool,
    subproblems :: p -> [(sc, d, p)]
}

ksDPProblem :: KSProblem -> DPProblem KSProblem Value (Maybe Item)
ksDPProblem p = DPProblem p isTrivial subproblems
    where
        isTrivial = null . items
        subproblems (KSP c (i : is))
            | value i <= c = [(0, Nothing, KSP c is), (value i, Just i, KSP (c - weight i) is)]
            | otherwise = [(0, Nothing, KSP c is)]
