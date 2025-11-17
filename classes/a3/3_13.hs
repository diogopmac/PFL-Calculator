group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = takeWhile (== x) (x:xs) : group (dropWhile (== x) xs)