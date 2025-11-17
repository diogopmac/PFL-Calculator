{- HAMMING NUMBERS UNTIL 2
[2^i*3^j*5^k | i<-[0..2], j<-[0..2], k<-[0..2]]

HAMMING NUMBERS INFINITE (NOT WORKING)
[2^i*3^j*5^k | i<-[0..], j<-[0..], k<-[0..]] -}

hamming :: Integer -> [Integer]
hamming n = [ 2^i*3^j*5^k | i<-[0..n], j<-[0..n], k<-[0..n], i+j+k == n]

hammingFunction :: [Integer]
hammingFunction = concatMap hamming [0..]