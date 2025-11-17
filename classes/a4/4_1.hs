calcpi1 :: Int -> Double
calcpi2 :: Int -> Double

calcpi1 n = sum (take n (zipWith (/) (cycle [4, -4]) [1,3..]))
calcpi2 n = 3 + sum (take n (zipWith (/) (cycle [4, -4]) [x*(x+1)*(x+2) | x<-[2,4..]]))