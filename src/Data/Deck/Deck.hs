module Data.Deck.Deck where

import Data.Card.Card

data Deck = Deck [Card]

deck :: Deck
deck = Deck $ Card <$> [One .. Ace] <*> [Hearts, Diamonds, Clubs, Spades]