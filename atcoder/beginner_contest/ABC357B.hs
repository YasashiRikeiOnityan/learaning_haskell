import Data.Char ( isUpper, isLower, toUpper, toLower )

main :: IO ()
main = do
    s <- getLine
    let tmp = solve s ((0,[]), (0, []))
    putStrLn $ if (fst . fst) tmp > (fst . snd) tmp then (snd . fst) tmp else (snd . snd) tmp

solve :: String -> ((Int, String), (Int, String)) -> ((Int, String), (Int, String))
solve [] p = p
solve (s : ss) ((i, u), (j, l))
    | isUpper s = solve ss ((succ i, u ++ [toUpper s]), (j, l ++ [toLower s]))
    | isLower s = solve ss ((i, u ++ [toUpper s]), (succ j, l ++ [toLower s]))