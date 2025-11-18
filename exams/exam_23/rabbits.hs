-- The rabbit population of the zoo increases every year. 
-- In year 0, there were 2 rabbits, while in year 1 there were 3 rabbits. 
-- In the following years, the number of rabbits corresponds to 
-- the sum of the rabbit population of the two previous years.

rabbits :: (Integral a) => [a]
rabbits = 2 : 3 : zipWith (+) rabbits (tail rabbits)

-- Implement rabbitYears :: (Integral a) => a -> Int, 
-- which returns the number of years needed for the rabbit population to be greater or equal
-- to the input integral value.

rabbitYears :: (Integral a) => a -> Int
rabbitYears 0 = 0
rabbitYears n = length (takeWhile (< n) rabbits)

