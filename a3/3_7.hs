toBits :: Int -> [Int]
toBits n = reverse (getDivisions n)

getDivisions :: Int -> [Int]
getDivisions 0 = [0]
getDivisions 1 = [1]
getDivisions n = (n `mod` 2) : getDivisions (n `div` 2)

