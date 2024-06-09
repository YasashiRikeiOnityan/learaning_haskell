type Capacity = Int
type Value = Int
type Weight = Int
data Item = Item { value :: Value, weight :: Weight }
data KSProblem = KSP { capacity :: Capacity, items :: [Item]}

type EDProblem = (String, String)
type Distance = Int

type Position = Int
type Step = Int
newtype Probability = Probability Double deriving (Show, Eq, Ord, Fractional, Num)
data RWProblem = RW { from :: Position, to :: Position, remaining :: Step }

class Semiring s where
    infixl 6 <+>
    (<+>) :: s -> s -> s
    infixl 7 <.>
    (<.>) :: s -> s -> s
    zero :: s
    one :: s

instance Semiring Probability where
    (<+>) = (+)
    (<.>) = (*)
    zero = 0
    one = 1

newtype TMin v = TMin v deriving (Eq, Ord, Show)
newtype TMax v = TMax v deriving (Eq, Ord, Show)

instance (Num v, Ord v, Bounded v) => Semiring (TMin v) where
    t1 <+> t2 = min t1 t2
    t1@(TMin v1) <.> t2@(TMin v2)
        | t1 == zero = zero
        | t2 == zero = zero
        | otherwise = TMin (v1 + v2)
    zero = TMin maxBound
    one = TMin 0