-- Implement substring :: (Integral a) => String -> a -> a -> String, 
-- which returns the substring of a given string between an initial and final
-- index (the character on the final index should also be included in the result; 
-- both indices are within bounds). Consider that the indices start at 0.
-- Constraint: You must solve this exercise using a list comprehension. 
-- Recursion and higher-order functions are prohibited.

substring :: (Integral a) => String -> a -> a -> String
substring str begin end = [ y | (x,y) <- zip [0..length str] str, x >= fromIntegral begin && x <= fromIntegral end]

hasSubstr :: String -> String -> Bool
hasSubstr str substr = hasSubstrAux str substr substr []

hasSubstrAux :: String -> String -> String -> String -> Bool
hasSubstrAux [] [] constsub acc = constsub == acc 
hasSubstrAux [] substr constsub acc = constsub == acc 
hasSubstrAux str [] constsub acc = constsub == acc 
hasSubstrAux (str:strs) (substr:substrs) constsub acc 
    | constsub == acc = True
    | str == substr = hasSubstrAux strs substrs constsub (acc ++ [substr])
    | otherwise = hasSubstrAux strs constsub constsub []