merge :: Ord a => [a] -> [a] -> [a]
merge [] b = b 
merge a [] = a
merge (x:xs) (y:ys) 
    | x <= y = x : merge xs (y:ys)
    | y < x = y : merge (x:xs) ys 

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
    where (left, right) = splitAt (length xs `div` 2) xs