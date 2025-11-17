{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}

type Match = ((String, String), (Int, Int))
type MatchDay = [Match]
type League = [MatchDay]

winner :: Match -> String
winner ((team1, team2), (score1, score2))
    | score1 > score2 = team1
    | score2 > score1 = team2
    | otherwise = "Draw"

matchDayScore :: String -> MatchDay -> Int
matchDayScore team matches
    | winner match == team = 3
    | winner match == "Draw" = 1
    | otherwise = 0
    where
        match = findMatch matches
        findMatch [] = (("", ""), (0,0))
        findMatch (x:xs)
            | fst (fst x) == team || snd (fst x) == team = x
            | otherwise = findMatch xs

matchDayScoreSimplified :: String -> MatchDay -> Int
matchDayScoreSimplified team [] = 0
matchDayScoreSimplified team (match:matches)
    | fst (fst match) == team || snd (fst match) == team =
        if winner match == team then 3
        else if winner match == "Draw" then 1
        else 0
    | otherwise = matchDayScoreSimplified team matches

numMatchDaysWithDraws :: League -> Int
numMatchDaysWithDraws [] = 0
numMatchDaysWithDraws (matchDay:matchDays)
    | isDraw matchDay = 1 + numMatchDaysWithDraws matchDays
    | otherwise = numMatchDaysWithDraws matchDays
    where
        isDraw [] = False
        isDraw (x:xs)
            | fst (snd x) == snd (snd x) = True
            | otherwise = isDraw xs



bigWins :: League -> [(Int, [String])]
bigWins [] = []
bigWins league = bigWinnersHelper league 1
    where 
        bigWinnersHelper [] _ = []
        bigWinnersHelper (matchDay:matchDays) id = (id, bigWinners matchDay) : bigWinnersHelper matchDays (id+1) 

        bigWinners [] = []
        bigWinners (x:xs) 
            | fst (snd x) - snd (snd x) >= 3 = fst(fst x) : bigWinners xs
            | snd (snd x) - fst (snd x) >= 3 = snd(fst x) : bigWinners xs
            | otherwise = bigWinners xs



