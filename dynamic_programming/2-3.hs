type Position = Int
type Step = Int
newtype Probability = Probability Double deriving (Show, Eq, Ord, Fractional, Num)
data RWProblem = RW { from :: Position, to :: Position, remaining :: Step }

randomWalk :: RWProblem -> Probability
randomWalk (RW p f s) 
    | p >= f = 1
    | s == 0 = 0
    | otherwise = sum [ 0.5 * randomWalk (RW (p + 1) f (s - 1))
                      , 0.5 * randomWalk (RW (p - 1) f (s - 1))
                      ]