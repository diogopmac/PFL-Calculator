isPrimeFast :: Integer -> Bool
isPrimeFast n 
    | n < 1 = False
    | n == 2 = True
    | otherwise = not (all (\x -> n `mod` x == 0) [2..floor (sqrt (fromIntegral n))])