nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (nubAux x xs)

nubAux :: Eq a => a -> [a] -> [a]
nubAux x [] = []
nubAux x (a:as)
        | x == a = nubAux x as
        | otherwise = a : nubAux x as