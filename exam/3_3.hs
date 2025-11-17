paragraphs :: String -> [String]
paragraphs [] = []
paragraphs txt = separateWords txt []
    where separateWords [] word = [word]
          separateWords [x] word = [word ++ [x]]
          separateWords (x:x1:xs) word
                | x == '\n' && x1 == '\n' = word : separateWords xs []
                | otherwise = separateWords (x1 : xs) (word ++ [x])

fillWords :: Int -> [String] -> [[String]]
fillWords n txt = fillWordsAux n txt []

fillWordsAux :: Int -> [String] -> [String] -> [[String]]
fillWordsAux n [] [] = []
fillWordsAux n [] line = [line]
fillWordsAux n (word:words) line
    | size == 0 = fillWordsAux n words [word]
    | size + 1 + length word <= n = fillWordsAux n words (line ++ [word])
    | otherwise = line : fillWordsAux n (word:words) []
    where
        size = sum (map length line) + length line - 1


wordsList :: String -> String -> [String]
wordsList [] word = [word]
wordsList (x:xs) word
    | x == ' ' = word : wordsList xs []
    | otherwise = wordsList xs (word ++ [x])

wordsThatFit :: Int -> [String] -> Int
wordsThatFit 0 [] = 0
wordsThatFit n [] = 0
wordsThatFit 0 txt = 0
wordsThatFit n (x:xs)
    | length x <= n = 1 + wordsThatFit (n - length x - 1) xs
    | otherwise = 0

main :: IO()
main = do
    input <- getContents
    let par = paragraphs input
    let word = map words par
    let formattedWords = map (fillWords 70) word
    let formattedLines = map (map unwords) formattedWords
    let output = unlines (map unlines formattedLines)
    putStr output

