module Data.Hand.Hand  where

import Data.Card.Card

type Unsorted = (Card, Card, Card, Card, Card)
data Hand = HighCard CardName
            | TwoOfAKind CardName
            | ThreeOfAKind CardName
            | FullHouse CardName CardName
            | FourOfAKind CardName
            | Straight CardName
            | Flush
            | StraightFlush Card deriving (Eq, Ord, Show)


highestHand :: (Card, Card, Card, Card, Card) -> (Card, Card) -> Hand
highestHand community player = undefined

unsortedHands :: (Card, Card, Card, Card, Card) -> (Card, Card) -> [Unsorted]
unsortedHands community player = undefined

possibleHands :: Unsorted -> [Hand]
possibleHands u = undefined
