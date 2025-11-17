{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

intercalate :: a -> [a] -> [[a]]
intercalate a [] = [[a]]
intercalate a list = map (\l -> intercalateAux a l list) [0..length list]

intercalateAux :: a -> Int -> [a] -> [a]
intercalateAux a 0 list = a : list
intercalateAux a n (x:xs) = x : intercalateAux a (n-1) xs

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concatMap (intercalate x) (permutations xs)