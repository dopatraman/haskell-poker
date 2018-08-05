module Data.Card.CardSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Card.Card

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Card" $ do
        it "compares two cards" $ do
            let card1 = Card Two Hearts
            let card2 = Card Three Hearts
            card1 < card2
