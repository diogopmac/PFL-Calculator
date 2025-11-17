classifyBMI :: Float -> Float -> String
classifyBMI w h 
    | b < 18.5 = "underweight"
    | b >= 18.5 && b < 25 = "normal weight"
    | b >= 25 && b < 30 = "overweight"
    | b >= 30 = "obese"
    where b = w / (h*h)