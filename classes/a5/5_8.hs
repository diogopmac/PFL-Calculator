booleans :: Int -> [[Bool]]
booleans 0 = [[]]
booleans n = [ b : bs | bs <- booleans (n-1), b <- [False, True]]