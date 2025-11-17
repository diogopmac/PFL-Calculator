paragraphs :: String -> [String]
paragraphs [] = []
paragraphs txt = separateWords txt []

separateWords :: String -> String -> [String]
separateWords [] word = [word]
separateWords [x] word = [word ++ [x]]
separateWords (x:x1:xs) word
    | x == '\n' && x1 == '\n' = word : separateWords xs []
    | otherwise = separateWords (x1 : xs) (word ++ [x])

paragraphs2 :: String -> [String]
paragraphs2 [] = []
paragraphs2 txt = separateWords txt []
    where separateWords [] word = [word]
          separateWords [x] word = [word ++ [x]]
          separateWords (x:x1:xs) word
                | x == '\n' && x1 == '\n' = word : separateWords xs []
                | otherwise = separateWords (x1 : xs) (word ++ [x])
