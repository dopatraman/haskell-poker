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
    describe "fullHouse" $ do
        it "should return a Maybe Hand" $ do
            let hand = (Card One Hearts,
                        Card One Spades,
                        Card Two Hearts,
                        Card Two Diamonds,
                        Card Two Clubs)
            fullHouse hand == Just (FullHouse Two One)
        it "should return Nothing" $ do
            let hand = (Card One Hearts,
                        Card One Spades,
                        Card Two Hearts,
                        Card Three Diamonds,
                        Card Jack Hearts)
            fullHouse hand == Nothing
    describe "fourOfAKind" $ do
        it "should return a Maybe Hand" $ do
            let hand = (Card One Hearts,
                        Card One Spades,
                        Card One Diamonds,
                        Card One Clubs,
                        Card Jack Hearts)
            fourOfAKind hand == Just (FourOfAKind One)
        it "should return Nothing" $ do
            let hand = (Card One Hearts,
                        Card One Spades,
                        Card One Diamonds,
                        Card Two Clubs,
                        Card Jack Hearts)
            fourOfAKind hand == Nothing
    describe "straight" $ do
        it "should return a Maybe Hand" $ do
            let hand = (Card One Hearts,
                        Card Two Hearts,
                        Card Three Clubs,
                        Card Four Diamonds,
                        Card Five Spades)
            straight hand == Just (Straight Five)
        it "should return a Maybe Hand for an unsorted list" $ do
            let hand = (Card One Hearts,
                        Card Three Clubs,
                        Card Two Hearts,
                        Card Four Diamonds,
                        Card Five Spades)
            straight hand == Just (Straight Five)
        it "should return Nothing" $ do
            let hand = (Card One Hearts,
                        Card One Spades,
                        Card One Diamonds,
                        Card Two Clubs,
                        Card Jack Hearts)
            straight hand == Nothing
    describe "flush" $ do
        it "should return a Maybe Hand" $ do
            let hand = (Card One Hearts,
                        Card Two Hearts,
                        Card Four Hearts,
                        Card Ten Hearts,
                        Card Jack Hearts)
            flush hand == Just Flush
        it "should return Nothing" $ do
            let hand = (Card One Hearts,
                        Card Two Hearts,
                        Card Four Hearts,
                        Card Ten Spades,
                        Card Jack Hearts)
            flush hand == Nothing
    describe "straightFlush" $ do
        it "should return a Maybe Hand" $ do
            pending
        it "should return Nothing" $ do
            pending