fromBits :: [Int] -> Int
fromBits [0] = 0
fromBits [1] = 1
fromBits (x:xs) = x * (2^(length(x:xs) - 1)) + fromBits xs