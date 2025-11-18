{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use second" #-}
type Species = (String, Int)
type Zoo = [Species]

-- Implement isEndangered :: Species -> Bool, which receives a species and determines 
-- if it is endangered. A species is considered endangered
-- if there are 100 or less individuals in the zoo.
isEndangered :: Species -> Bool
isEndangered (name, num) = num <= 100

-- Implement updateSpecies :: Species -> Int -> Species, which, 
-- given a Species and an amount of newborn babies, returns a new instance of
-- Species with the updated population.

updateSpecies :: Species -> Int -> Species
updateSpecies (name, num) babies = (name, num+babies)

-- Implement filterSpecies :: Zoo -> (Species -> Bool) -> Zoo, which, 
-- given the list species of a zoo and a predicate (i.e. a function that performs
-- a test on each species), returns the sublist of species that satisfy the predicate. 
-- The order of the species in the result must be the same as in the input.

filterSpecies :: Zoo -> (Species -> Bool) -> Zoo
filterSpecies [] funct = []
filterSpecies (zoo:zoos) funct
    | funct zoo = zoo : filterSpecies zoos funct
    | otherwise = filterSpecies zoos funct

-- Implement countAnimals :: Zoo -> Int, which, given the list of species of a zoo, 
-- counts the total population of the zoo.
-- Constraint: You must solve this exercise using higher-order functions. 
-- Recursion and list comprehensions are prohibited.

countAnimals :: Zoo -> Int
countAnimals zoo = sum (map snd zoo)

-- Implement sortSpeciesWithSubstr :: Zoo -> String -> (Zoo, Zoo), 
-- which divides the species of the Zoo into a pair of sublists. 
-- The first sublist stores all the species whose name contains 
-- the string argument, while the second list has the remaining species. 
-- The order of the species in each list of the resulting pair must match the input list.

hasSubstr :: String -> String -> Bool
hasSubstr _ [] = True
hasSubstr str substr = substr `elem` ([take len (drop i str) | i <- [0..length str - len]])
    where len = length substr

sortSpeciesWithSubstr :: Zoo -> String -> (Zoo, Zoo)
sortSpeciesWithSubstr [] _ = ([], [])
sortSpeciesWithSubstr ((species, num):zoos) str
    | hasSubstr species str = ((species,num) : fst (sortSpeciesWithSubstr zoos str), snd (sortSpeciesWithSubstr zoos str))
    | otherwise = (fst (sortSpeciesWithSubstr zoos str), (species,num) : snd (sortSpeciesWithSubstr zoos str))






