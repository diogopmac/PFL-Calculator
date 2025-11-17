import Data.List
data Suit = Clubs | Spades | Hearts | Diamonds
    deriving (Show, Eq, Ord)

suitValue :: Suit -> Int
suitValue Clubs = 1
suitValue Spades = 2
suitValue Hearts = 3
suitValue Diamonds = 4


data Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Q | J | K
    deriving (Show, Eq, Ord)

faceValue :: Face -> Int
faceValue Two = 2
faceValue Three = 3
faceValue Four = 4
faceValue Five = 5
faceValue Six = 6
faceValue Seven = 7
faceValue Eight = 8
faceValue Nine = 9
faceValue Ten = 10
faceValue J = 11
faceValue Q = 12
faceValue K = 13
faceValue Ace = 14

data Card = Card Suit Face
    deriving Show

allCards :: [Card]
allCards = [ Card suit face | suit <- [Clubs, Spades, Hearts, Diamonds], face <- [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Q, J, K]]

cmp1 :: Card -> Card -> Ordering
cmp1 (Card s1 f1) (Card s2 f2)
    | suitValue s1 > suitValue s2 = GT
    | suitValue s2 > suitValue s1 = LT 
    | faceValue f1 > faceValue f2 = GT
    | faceValue f2 > faceValue f1 = LT
    | otherwise = EQ

cmp2 :: Card -> Card -> Ordering
cmp2 (Card s1 f1) (Card s2 f2)
    | faceValue f1 > faceValue f2 = GT
    | faceValue f2 > faceValue f1 = LT
    | suitValue s1 > suitValue s2 = GT
    | suitValue s2 > suitValue s1 = LT 
    | otherwise = EQ