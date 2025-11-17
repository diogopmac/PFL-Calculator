binom :: Integer -> Integer -> Integer
binom n k = factorial n `div` (factorial k * factorial (n - k))

pascal :: Integer -> [[Integer]]
pascal n = [ [binom x y | y <- [0..x]] | x <- [0..n] ]

factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)
