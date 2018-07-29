module Data.Hand.HandSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Card.Card
import Data.Hand.Hand

spec :: Spec
spec = do
    describe "twoOfAKind" $ do
        it "should return a Maybe Hand" $ do
            let hand = (Card One Hearts,
                        Card One Spades,
                        Card Two Hearts,
                        Card Three Diamonds,
                        Card Jack Hearts)
            twoOfAKind hand == Just (TwoOfAKind One)
        it "should return Nothing" $ do
            let hand = (Card One Hearts,
                        Card Four Spades,
                        Card Two Hearts,
                        Card Three Diamonds,
                        Card Jack Hearts)
            twoOfAKind hand == Nothing
    describe "threeOfAKind" $ do
        it "should return a Maybe Hand" $ do
            let hand = (Card One Hearts,
                        Card One Spades,
                        Card One Diamonds,
                        Card Three Diamonds,
                        Card Jack Hearts)
            threeOfAKind hand == Just (ThreeOfAKind One)
        it "should return Nothing" $ do
            let hand = (Card One Hearts,
                        Card One Spades,
                        Card Two Hearts,
                        Card Three Diamonds,
                        Card Jack Hearts)
            threeOfAKind hand == Nothing
