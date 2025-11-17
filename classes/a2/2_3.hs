{- a -}
max3, min3 :: Ord a => a -> a -> a -> a
max3 x y z 
    | x >= y && x >= z = x 
    | y >= x && y >= z = y 
    | z >= x && z >= y = z
min3 x y z 
    | x <= y && x <= z = x 
    | y <= x && y <= z = y 
    | z <= x && z <= y = z

{- b -}
maximum3, minimum3 :: Ord a => a -> a -> a -> a
maximum3 x y z = max (max x y) z
minimum3 x y z = min (min x y) z

