data Suit = Clubs | Spades | Hearts | Diamonds
    deriving Show

data Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Q | J | K 
    deriving Show 

data Card = Card Suit Face

allCards :: [Card]
allCards = [ Card suit face | suit <- [Clubs, Spades, Hearts, Diamonds], face <- [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Q, J, K]]