leastDiv :: Integer -> Integer
leastDiv 1 = 1
leastDiv n = findDiv n 2
    where findDiv n d
            | d * d > n = n
            | n `mod` d == 0 = d
            | otherwise = findDiv n (d+1)

isPrimeFast :: Integer -> Bool
isPrimeFast 1 = True
isPrimeFast n = leastDiv n == n
