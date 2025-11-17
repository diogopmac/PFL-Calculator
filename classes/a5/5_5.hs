import Set

type Dict = Set String

readDict :: IO [String]
readDict = do
    txt <- readFile "/usr/share/dict/words"
    return (words txt)

checkWord :: Dict -> String -> String
checkWord dict string
    | member string dict = string
    | otherwise = "\ESC[7m" ++ string ++ "\ESC[0m"

spellCheck :: Dict -> String -> String
spellCheck dict text = unwords (map (checkWord dict) (words text))

dictSpellCheck :: IO()
dictSpellCheck = do
    dict <- readDict
    input <- getContents
    putStrLn (spellCheck (fromList dict) input)