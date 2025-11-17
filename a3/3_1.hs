myand :: [Bool] -> Bool
myand [] = False
myand (x:xs) = x && myand xs

myor :: [Bool] -> Bool
myor [] = False
myor (x:xs) = x || myor xs

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ concat xs

myreplicate :: Int -> a -> [a]
myreplicate 0 a = []
myreplicate n a = [a] ++ myreplicate (n-1) a

(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x
(!!!) (x:xs) n = (!!!) xs (n-1)

myelem :: Eq a => a -> [a] -> Bool
myelem a [] = False
myelem a (x:xs) = (a == x) || myelem a xs

