divisors :: Integer -> [Integer]
divisors n = filter (\x -> n `mod` x == 0) [1..n]