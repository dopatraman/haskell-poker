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
            pipeline unsorted = map ($ unsorted) cardFns
            filterNothings maybeList = filter (\x -> x /= Nothing) maybeList
            getValues maybeList = map just maybeList
            just m = case m of Just a -> a

-----------------------------------------------------------------

twoOfAKind :: Unsorted -> Maybe Hand
twoOfAKind u
    | (not . null) matches = Just (TwoOfAKind (cardName . head $ matches))
    | otherwise = Nothing
    where   pairs = combos (toList u) 2
            matches = concat $ dropWhile (not . same) $ pairs
            cardName (Card name _) = name

threeOfAKind :: Unsorted -> Maybe Hand
threeOfAKind u
    | (not . null) matches = Just (ThreeOfAKind (cardName . head $ matches))
    | otherwise = Nothing
    where   pairs = combos (toList u) 3
            matches = concat $ dropWhile (not . same) $ pairs
            cardName (Card name _) = name

fullHouse :: Unsorted -> Maybe Hand
fullHouse u
    | k2 /= Nothing && k3 /= Nothing && namek2 /= namek3 = Just (FullHouse namek3 namek2)
    | otherwise = Nothing
    where   k2 = twoOfAKind u
            k3 = threeOfAKind u
            name2 (Just (TwoOfAKind name)) = name
            name3 (Just (ThreeOfAKind name)) = name
            namek2 = name2 k2
            namek3 = name3 k3

toList :: Unsorted -> [Card]
toList (f1, f2, f3, turn, riv) = [f1, f2, f3, turn, riv]

same :: Eq a => [a] -> Bool
same xs = all (== head xs) (tail xs)