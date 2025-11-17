{- a -}
(+++) :: [a] -> [a] -> [a]
(+++) l1 l2 = foldr (:) l2 l1

{- b -}
concat :: [[a]] -> [a]
concat = foldr (+++) []

{- c -}
reverseFl :: [a] -> [a]
reverseFl = foldl (\ acc x -> x : acc) []
{- O(n) -}

{- d -}
reverseFr :: [a] -> [a]
reverseFr = foldr (\ x acc -> acc ++ [x]) []
{- O(n2) -}
