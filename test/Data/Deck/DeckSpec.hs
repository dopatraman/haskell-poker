module Data.Deck.DeckSpec (spec) where

import Data.Deck.Deck
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "deck" $ do
        it "should return a full deck" $ do
            (length . cards $ deck) == (4 * 13)