{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
    
short :: [a] -> Bool
short a = length a < 3

short2 :: [a] -> Bool
short2 a = if length a < 3 then True else False