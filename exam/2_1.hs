maxIndex :: Ord a => [a] -> (a, Int)
maxIndex (a:as) = maxIndexAux as (a, 0) 1

maxIndexAux :: Ord a => [a] -> (a, Int) -> Int -> (a, Int)
maxIndexAux [] acc _ = acc
maxIndexAux (a:as) (maxVal, maxInd) ind
    | a >= maxVal = maxIndexAux as (a, ind) (ind + 1)
    | otherwise    = maxIndexAux as (maxVal, maxInd) (ind + 1)