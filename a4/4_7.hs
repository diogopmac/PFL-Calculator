type Dict = [String]
readDict :: IO Dict
readDict = do
    txt <- readFile "/usr/share/dict/words"
    return (words txt)

a :: IO Int
a = do
    length <$> readDict

checkWord :: Dict -> String -> String
checkWord dict string
    | string `elem` dict = string
    | otherwise = "\ESC[7m" ++ string ++ "\ESC[0m"

spellCheck :: Dict -> String -> String
spellCheck dict text = unwords (map (checkWord dict) (words text))

dictSpellCheck :: IO()
dictSpellCheck = do
    dict <- readDict
    input <- getContents
    putStrLn (spellCheck dict input)
