module Table where

import Data.List
import Data.Deck.Deck
import Data.Hand.Hand

dealAll :: Deck -> Int -> (Deck, [(Card, Card)])
dealAll d n = dealAll' d n []
    where   dealAll' d@(Deck []) _ acc = (d, acc)
            dealAll' d 0 acc = (d, acc)
            dealAll' d n acc = dealAll' (fst . dealHand d) (n-1) ((snd . dealHand d):acc)

round :: Deck -> Int -> (Card, Card, Card, Card, Card) -> [Hand]
round d players community = map (\x -> c x) (dealAll d players)
    where c = playedHand community