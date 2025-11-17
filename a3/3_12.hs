fromBits :: [Int] -> Int
fromBits = foldl (\acc bit -> acc*2 + bit) 0