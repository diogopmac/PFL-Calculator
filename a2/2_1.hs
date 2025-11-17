classify :: Int -> String
classify x
    | x < 10 = "failed"
    | x > 9 && x < 13 = "passed"
    | x > 12 && x < 16 = "good"
    | x > 15 && x < 19 = "very good"
    | x > 18 && x < 21 = "excelent"
    | otherwise = "invalid"