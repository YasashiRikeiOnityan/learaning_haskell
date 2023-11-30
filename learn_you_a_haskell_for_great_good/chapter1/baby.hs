doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = x * 2 + y * 2

-- You need to write an "else" clause.
doubleSmallerNumber :: (Ord a, Num a) => a -> a
doubleSmallerNumber x = if x > 100
    then x
    else x * 2

doubleSmallerNumber' :: (Ord a, Num a) => a -> a
doubleSmallerNumber' x = (if x > 100 then x else x * 2) + 1