type Name = Char -- 'x', 'y', 'z', etc
type Env = [(Name, Bool)]

data Prop = Const Bool
    | Var Name -- variable
    | Not Prop -- ¬ A 
    | And Prop Prop -- A ∧ B
    | Or Prop Prop -- A ∨ B
    | Imply Prop Prop -- A -=> B

eval :: Env -> Prop -> Bool
eval env (Const b) = b
eval env (Var x)
    = case lookup x env of
        Just b -> b
        Nothing -> error "undefined variable"
eval env (Not p) = not (eval env p)
eval env (And p q )
    = eval env p && eval env q
eval env (Imply p q)
    = not (eval env p) || eval env q
eval env (Or p q)
    = eval env p || eval env q

vars :: Prop -> [Name]
vars (Const x) = []
vars (Var x) = [x]
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Not x) = vars x
vars (Imply p q) = vars p ++ vars q

booleans :: Int -> [[Bool]]
booleans 0 = [[]]
booleans n = [ b : bs | bs <- booleans (n-1), b <- [False, True]]

environments :: [Name] -> [Env]
environments list = map (zip list) (booleans (length list))

table :: Prop -> [(Env, Bool)]
table prop = [ (x , eval x prop) | x <- environments(vars prop)]

