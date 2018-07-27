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
                | Ace deriving (Eq, Ord, Enum, Show)

data Suite = Hearts | Diamonds | Spades | Clubs deriving (Eq, Show)

data Card = Card CardName Suite deriving (Eq, Show)

instance Ord Card where
    (Card namex _) `compare` (Card namey _) = namex `compare` namey
