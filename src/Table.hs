module Table where

import Data.List
import Data.Deck.Deck
import Data.Hand.Hand
import Data.Card.Card

dealAll :: Deck -> Int -> (Deck, [(Card, Card)])
dealAll d n = dealAll' d n []
    where   dealAll' d@(Deck []) _ acc = (d, acc)
            dealAll' d 0 acc = (d, acc)
            dealAll' d n acc = dealAll' (fst (dealHand d)) (n-1) ((snd (dealHand d)):acc)

round :: Deck -> Int -> ((Card, Card), Hand)
round d players = do
    let (d', playerHands) = dealAll d players
    let (d'', communityCards) = dealCommunity d'
    let playedHands = map (\x -> playedHand communityCards x) playerHands
    let playedPairs = zip playerHands playedHands
    let winningHand = maximum playedHands
    let (winningPlayer, _) = head $ filter (\x -> (snd x) == winningHand) playedPairs
    (winningPlayer, winningHand)