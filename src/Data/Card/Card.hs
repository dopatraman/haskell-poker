module Data.Card.Card   where

data CardName = Joker
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

data Suit = Hearts | Diamonds | Spades | Clubs deriving (Eq, Show)

data Card = Card CardName Suit deriving (Show)

cardName :: Card -> CardName
cardName (Card name _) = name

suit :: Card -> Suit
suit (Card _ s) = s

instance Eq Card where
    Card namex _ == Card namey _ = namex == namey

instance Ord Card where
    (Card namex _) `compare` (Card namey _) = namex `compare` namey
