myconcat :: [a] -> [a] -> [a]
myconcat a b = [ x | l <- [a, b], x <- l]