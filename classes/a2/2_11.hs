isPrime :: Integer -> Bool
isPrime n = null [ x | x <- [2..n-1], n `mod` x == 0 ]