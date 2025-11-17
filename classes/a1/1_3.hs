second :: [a] -> a
second l = head (tail l)

last :: [a] -> a
last l = head (reverse l)

init :: [a] -> [a]
init l = reverse (tail (reverse l))

middle :: [a] -> a
middle a = head (drop half a)
    where half = div (length a) 2

checkPallindrome :: String -> Bool
checkPallindrome l = l == reverse l