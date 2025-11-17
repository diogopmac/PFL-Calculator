insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | y >= x = x:y:ys
                | otherwise = y: insert x ys

isSort :: Ord a => [a] -> [a]
isSort [] = [];
isSort (x:xs) = insert x (isSort xs)