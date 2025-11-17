type Name = Char -- 'x', 'y', 'z', etc
type Env = [(Name, Bool)]

booleans :: Int -> [[Bool]]
booleans 0 = [[]]
booleans n = [ b : bs | bs <- booleans (n-1), b <- [False, True]]

environments :: [Name] -> [Env]
environments list = map (zip list) (booleans (length list))