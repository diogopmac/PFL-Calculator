{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use pragma syntax" #-}
incr :: Int -> Int
incr x = x+1

triple :: Int -> Int
triple x = 3*x

welcome :: String -> String
welcome name = "Hello, " ++ name ++ "!"

count :: String -> String
count str = show (length str) ++ " characters."

{- 
(a) incr (triple 3)
(b) triple (incr (3+1))
(c) triple (incr 3 + 1)
(d) triple (incr 3) + 1
(e) welcome "Harry" ++ welcome "Potter"
(f) welcome ("Harry" ++ " Potter")
(g) welcome (welcome "Potter")
(h) count "Expelliarmus!"
(i) count (count "Expelliarmus!")
-}