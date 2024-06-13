import Data.Char ( isUpper, isLower, toUpper, toLower )

main :: IO ()
main = do
    s <- getLine
    let ((x, s), (y, t)) = solve s ((0,[]), (0, []))
    putStrLn $ if x > y then s else t

solve :: String -> ((Int, String), (Int, String)) -> ((Int, String), (Int, String))
solve [] p = p
solve (s : ss) ((i, u), (j, l))
    | isUpper s = solve ss ((succ i, u ++ [toUpper s]), (j, l ++ [toLower s]))
    | isLower s = solve ss ((i, u ++ [toUpper s]), (succ j, l ++ [toLower s]))