-- asパターンは，値を分解しつつ，パターンマッチの対象になった
-- 値自体も参照したいときに使います．

firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x] 