module Data.Deck.Deck where

import Data.Card.Card

data Deck = Deck [Card]

deck :: Deck
deck = Deck $ Card <$> [One .. Ace] <*> [Hearts, Diamonds, Clubs, Spades]

shuffle :: Deck -> Deck
shuffle d = undefined

cards :: Deck -> [Card]
cards (Deck x) = x
cards _ = []

dealHand :: Deck -> (Deck, (Card, Card))
dealHand d = ((drop 2 cards d), make . playerHand d)
    where   make (x:y:[]) = (x, y)
            playerHand = take 2 (cards d)

dealCommunity :: Deck -> (Deck, (Card, Card, Card, Card, Card))
dealCommunity d = ((drop 5 cards d), make . community d)
    where   make (a:b:c:dd:e:f:[]) = (a, b, c, dd, e, f)
            community = take 5 (cards d)