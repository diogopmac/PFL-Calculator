increment :: [Int] -> Int -> [Int]
increment [] ind = []
increment (a:as) ind
    | ind == 0 = (a+1) : as
    | otherwise = a : increment as (ind-1)

maxIndex :: Ord a => [a] -> (a, Int)
maxIndex (a:as) = maxIndexAux as (a, 0) 1

maxIndexAux :: Ord a => [a] -> (a, Int) -> Int -> (a, Int)
maxIndexAux [] acc _ = acc
maxIndexAux (a:as) (maxVal, maxInd) ind
    | a >= maxVal = maxIndexAux as (a, ind) (ind + 1)
    | otherwise    = maxIndexAux as (maxVal, maxInd) (ind + 1)

hondtCalc :: [Int] -> [Int] -> [Int]
hondtCalc [] [] = []
hondtCalc [] _ = []
hondtCalc _ [] = [] 
hondtCalc (x:xs) (y:ys) = (y `div` (x + 1)) : hondtCalc xs ys 

hondt :: Int -> [Int] -> [Int]
hondt n list = hondtAux n [0 | _ <- [1..length list]] list 

hondtAux :: Int -> [Int] -> [Int] -> [Int]
hondtAux 0 dep votes = dep
hondtAux n dep votes = hondtAux (n-1) (increment dep (snd(maxIndex(hondtCalc dep votes)))) votes