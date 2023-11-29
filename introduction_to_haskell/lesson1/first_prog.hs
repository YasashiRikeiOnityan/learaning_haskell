main :: IO ()
main = do
    print "Who is email for?"
    recipient <- getLine :: IO String
    print "What is the Title?"
    title <- getLine :: IO String
    print "Who is the Author?"
    author <- getLine :: IO String
    print $ createEmail recipient title author

createEmail :: String -> String -> String -> String
createEmail recipient title author = 
    toPart recipient ++
    bodyPart title ++
    fromPart author

toPart :: String -> String
toPart recipient = "Dear " ++ recipient ++ ",\n"

bodyPart :: String -> String
bodyPart title = "Thanks for buying " ++ title ++ ".\n"

fromPart :: String -> String
fromPart author = "Thanks,\n" ++ author