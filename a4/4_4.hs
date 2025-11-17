merge :: [Integer] -> [Integer] -> [Integer]
merge [] b = b 
merge a [] = a
merge (x:xs) (y:ys) 
    | x < y = x : merge xs (y:ys)
    | y < x = y : merge (x:xs) ys 
    | otherwise = x : merge xs ys

hamming :: [Integer]
hamming = 1 : merge (map (*2) hamming) (merge (map (*3) hamming) (map (*5) hamming))