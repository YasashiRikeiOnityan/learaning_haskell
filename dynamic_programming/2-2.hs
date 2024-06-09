type EDProblem = (String, String)
type Distance = Int

editDistance :: EDProblem -> Distance
editDistance ([], []) = 0
editDistance (x : xs, []) = 1 + editDistance (xs, [])
editDistance ([], y : ys) = 1 + editDistance ([], ys)
editDistance (x : xs, y : ys) = 
    minimum [
        s + editDistance (xs, ys),
        1 + editDistance (x : xs, ys),
        1 + editDistance (xs, y : ys)
    ] where s = if x == y then 0 else 1