module Data.Hand.Hand  where

import Data.Card.Card
import Data.Hand.Unigraph
import Data.List

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
    where   trips = combos (toList u) 3
            matches = concat $ dropWhile (not . same) $ trips
            cardName (Card name _) = name

fullHouse :: Unsorted -> Maybe Hand
fullHouse u
    | k2 /= Nothing && k3 /= Nothing && (name k2) /= (name k3) = Just (FullHouse (name k3) (name k2))
    | otherwise = Nothing
    where   k2 = twoOfAKind u
            k3 = threeOfAKind u
            cardName (TwoOfAKind name) = name
            cardName (ThreeOfAKind name) = name
            name k = maybe Joker (\x -> cardName x) k

fourOfAKind :: Unsorted -> Maybe Hand
fourOfAKind u
    | (not . null) matches = Just (FourOfAKind (cardName . head $ matches))
    | otherwise = Nothing
    where   quads = combos (toList u) 4
            matches = concat $ dropWhile (not . same) $ quads
            cardName (Card name _) = name

straight :: Unsorted -> Maybe Hand
straight u
    | isConsecutive (map value cards) = Just (Straight (name . last $ cards))
    | otherwise = Nothing
    where   cards = sort $ toList u
            name (Card name _) = name
            value c = fromEnum (name c)

flush :: Unsorted -> Maybe Hand
flush u
    | same suits = Just Flush
    | otherwise = Nothing
    where   suits = map suit (toList u)
            suit (Card _ s) = s

straightFlush :: Unsorted -> Maybe Hand
straightFlush u 
    | (isStraight u) && (isFlush u) = Just (StraightFlush (last cards))
    | otherwise = Nothing
    where   cards = toList u
            isStraight = isX . straight
            isFlush = isX . flush
            isX (Just _) = True
            isX Nothing = False

isConsecutive :: (Eq a, Num a) => [a] -> Bool
isConsecutive xs = isConsecutive' xs False
    where   isConsecutive' [] acc = acc
            isConsecutive' (x:[]) acc = acc
            isConsecutive' (x:y:ys) acc
                | x + 1 == y = isConsecutive' (y:ys) True
                | otherwise = False

toList :: Unsorted -> [Card]
toList (f1, f2, f3, turn, riv) = [f1, f2, f3, turn, riv]

same :: Eq a => [a] -> Bool
same xs = all (== head xs) (tail xs)