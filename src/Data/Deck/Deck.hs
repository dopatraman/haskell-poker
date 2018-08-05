module Data.Deck.Deck where

import Data.Card.Card

data Deck = Deck [Card]

deck :: Deck
deck = Deck $ Card <$> [Two .. Ace] <*> [Hearts, Diamonds, Clubs, Spades]

shuffle :: Deck -> Deck
shuffle d = undefined

cards :: Deck -> [Card]
cards (Deck x) = x

uncards :: [Card] -> Deck
uncards xs = Deck xs

dealHand :: Deck -> (Deck, (Card, Card))
dealHand d = (uncards restOfDeck, make playerHand)
    where   make (x:y:[]) = (x, y)
            restOfDeck = drop 2 (cards d)
            playerHand = take 2 (cards d)

dealCommunity :: Deck -> (Deck, (Card, Card, Card, Card, Card))
dealCommunity d = (uncards restOfDeck, make community)
    where   make (a:b:c:dd:e:[]) = (a, b, c, dd, e)
            restOfDeck = drop 5 (cards d)
            community = take 5 (cards d)