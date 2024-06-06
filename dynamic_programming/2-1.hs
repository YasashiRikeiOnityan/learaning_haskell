type Capacity = Int
type Value = Int
type Weight = Int
data Item = Item { value :: Value, weight :: Weight }
data KSProblem = KSP { capacity :: Capacity, items :: [Item]}

knapsack :: KSProblem -> Value
knapsack (KSP _ []) = 0
knapsack (KSP c (i : is))
    | weight i <= c = max (knapsack (KSP c is))
                          (value i + knapsack (KSP (c - weight i) is))
    | otherwise = knapsack (KSP c is)
