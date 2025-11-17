import Data.Char

convertRot13 :: Char -> Char
convertRot13 x
    | isUpper x = chr(((ord x - ord 'A' + 13) `mod` 26) + ord 'A')
    | isLower x = chr(((ord x - ord 'a' + 13) `mod` 26) + ord 'a')
    | otherwise = x

rot13 :: IO ()
rot13 = do 
    input <- getLine
    let output = map convertRot13 input
    putStrLn output
