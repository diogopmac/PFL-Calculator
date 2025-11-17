type AWord = String
type Line = [AWord]
type Paragraph = [Line]

fillWords :: Int -> [AWord] -> Paragraph
fillWords n = fillWordsAux n [] 0

fillWordsAux :: Int -> [AWord] -> Int -> [AWord] -> Paragraph
fillWordsAux 0 _ _ _ = []
fillWordsAux _ [] _ [] = []
fillWordsAux _ currentLine _ [] = [currentLine]
fillWordsAux n currentLine cur (x:xs)
    | (length x + cur) < n = fillWordsAux n (currentLine ++ [x]) (cur + length x) xs
    | otherwise = currentLine : fillWordsAux n [x] (length x) xs

readWords :: IO ()
readWords = do
    input <- getLine
    let output = fillWords 20 (words input)
    mapM_ putStrLn (map unwords output)