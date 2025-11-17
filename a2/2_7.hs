median :: Ord a => a -> a -> a -> a
median x y z 
    | (y > x && x > z) || (z > x && x > y) = x
    | (x > y && y > z) || (z > y && y > x) = y
    | otherwise = z 

median3 :: Real a => a -> a -> a -> a
median3 x y z = (x+y+z) - max(max x y) z - min (min x y) z