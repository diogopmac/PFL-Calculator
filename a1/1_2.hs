halves :: [a] -> ([a], [a])
halves l = (take half l, drop half l)
    where half = div (length l) 2
