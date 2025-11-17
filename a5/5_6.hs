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
