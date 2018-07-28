module Data.Hand.Hand  where

import Data.Card.Card
import Data.Hand.Unigraph

type Unsorted = (Card, Card, Card, Card, Card)
data Hand = HighCard CardName
            | TwoOfAKind CardName
            | ThreeOfAKind CardName
            | FullHouse CardName CardName
            | FourOfAKind CardName
            | Straight CardName
            | Flush
            | StraightFlush Card deriving (Eq, Ord, Show)

playedHand :: (Card, Card, Card, Card, Card) -> (Card, Card) -> Hand
playedHand community player = maximum $ map highestHand (unsortedHands community player)

unsortedHands :: (Card, Card, Card, Card, Card) -> (Card, Card) -> [Unsorted]
unsortedHands community player = undefined

highestHand :: Unsorted -> Hand
highestHand u = maximum $ possibleHands u

possibleHands :: Unsorted -> [Hand]
possibleHands u = getValues $ filterNothings $ pipeline $ toList u
    where   cardFns = undefined
            toList tup = undefined
            pipeline unsorted = map ($ unsorted) cardFns
            filterNothings maybeList = filter (\x -> x /= Nothing) maybeList
            getValues maybeList = map just maybeList
            just m = case m of Just a -> a