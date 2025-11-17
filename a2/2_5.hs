safetail :: [a] -> [a]
safetail a
    | null a = a
    | not (null a) = tail a

safetailCond :: [a] -> [a]
safetailCond [] = []
safetailCond a = tail a

safetailIf :: [a] -> [a]
safetailIf a = if null a then [] else tail a