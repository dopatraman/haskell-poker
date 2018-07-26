module Data.Card.Card   where

data CardName = One
                | Two
                | Three
                | Four
                | Five
                | Six
                | Seven
                | Eight
                | Nine
                | Ten
                | Jack
                | Queen
                | King
                | Ace deriving (Eq, Ord, Enum)

data Suite = Hearts | Diamonds | Spades | Clubs deriving (Eq)

data Card = Card CardName Suite

instance Eq Card where
    Card namex suitex == Card namey suitey = (namex == namey) && (suitex == suitey)

instance Ord Card where
    (Card namex _) `compare` (Card namey _) = namex `compare` namey
