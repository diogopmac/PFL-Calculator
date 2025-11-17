intersperse :: a -> [a] -> [a]
intersperse a [c] = [c]
intersperse a [] = []
intersperse a (x:xs) = x : a : intersperse a xs  