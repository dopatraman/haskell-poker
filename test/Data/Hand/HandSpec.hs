module Data.Hand.HandSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Card.Card
import Data.Hand.Hand

spec :: Spec
spec = do
    describe "highestHand" $ do
        it "should get the highest hand for a set of 5 Cards" $ do
            let hand = [Card Six Hearts,
                        Card Six Spades,
                        Card Two Hearts,
                        Card Three Diamonds,
                        Card Jack Hearts]
            highestHand hand == TwoOfAKind Six
        it "should get the highest hand out of possible hands" $ do
            let hand = [Card Five Hearts,
                        Card Five Spades,
                        Card Ten Clubs,
                        Card Ten Diamonds,
                        Card Ten Hearts]
            highestHand hand === FullHouse Ten Five
        it "should get the highest hand when cards are out of order" $ do
            let hand = [Card Ten Hearts,
                        Card Jack Diamonds,
                        Card Five Spades,
                        Card Five Hearts,
                        Card Ace Spades]
            highestHand hand == TwoOfAKind Five
        it "should get the highest straight out of order" $ do
            let hand = [Card Ace Spades,
                        Card Jack Diamonds,
                        Card Ten Clubs,
                        Card King Hearts,
                        Card Queen Hearts]
            highestHand hand == Straight Ace
    describe "twoOfAKind" $ do
        it "should return a Maybe Hand" $ do
            let hand = [Card Six Hearts,
                        Card Six Spades,
                        Card Two Hearts,
                        Card Three Diamonds,
                        Card Jack Hearts]
            twoOfAKind hand == Just (TwoOfAKind Six)
        it "should return Nothing" $ do
            let hand = [Card Six Hearts,
                        Card Four Spades,
                        Card Two Hearts,
                        Card Three Diamonds,
                        Card Jack Hearts]
            twoOfAKind hand == Nothing
    describe "threeOfAKind" $ do
        it "should return a Maybe Hand" $ do
            let hand = [Card Six Hearts,
                        Card Six Spades,
                        Card Six Diamonds,
                        Card Three Diamonds,
                        Card Jack Hearts]
            threeOfAKind hand == Just (ThreeOfAKind Six)
        it "should return Nothing" $ do
            let hand = [Card Six Hearts,
                        Card Six Spades,
                        Card Two Hearts,
                        Card Three Diamonds,
                        Card Jack Hearts]
            threeOfAKind hand == Nothing
    describe "fullHouse" $ do
        it "should return a Maybe Hand" $ do
            let hand = [Card Six Hearts,
                        Card Six Spades,
                        Card Two Hearts,
                        Card Two Diamonds,
                        Card Two Clubs]
            fullHouse hand == Just (FullHouse Two Six)
        it "should return Nothing" $ do
            let hand = [Card Six Hearts,
                        Card Six Spades,
                        Card Two Hearts,
                        Card Three Diamonds,
                        Card Jack Hearts]
            fullHouse hand == Nothing
    describe "fourOfAKind" $ do
        it "should return a Maybe Hand" $ do
            let hand = [Card Six Hearts,
                        Card Six Spades,
                        Card Six Diamonds,
                        Card Six Clubs,
                        Card Jack Hearts]
            fourOfAKind hand == Just (FourOfAKind Six)
        it "should return Nothing" $ do
            let hand = [Card Six Hearts,
                        Card Six Spades,
                        Card Six Diamonds,
                        Card Two Clubs,
                        Card Jack Hearts]
            fourOfAKind hand == Nothing
    describe "straight" $ do
        it "should return a Maybe Hand" $ do
            let hand = [Card Two Hearts,
                        Card Three Clubs,
                        Card Four Diamonds,
                        Card Five Spades,
                        Card Six Spades]
            straight hand == Just (Straight Six)
        it "should return a Maybe Hand for an unsorted list" $ do
            let hand = [Card Three Clubs,
                        Card Two Hearts,
                        Card Four Diamonds,
                        Card Five Spades,
                        Card Six Hearts]
            straight hand == Just (Straight Six)
        it "should return Nothing" $ do
            let hand = [Card Six Hearts,
                        Card Six Spades,
                        Card Six Diamonds,
                        Card Two Clubs,
                        Card Jack Hearts]
            straight hand == Nothing
    describe "flush" $ do
        it "should return a Maybe Hand" $ do
            let hand = [Card Six Hearts,
                        Card Two Hearts,
                        Card Four Hearts,
                        Card Ten Hearts,
                        Card Jack Hearts]
            flush hand == Just Flush
        it "should return Nothing" $ do
            let hand = [Card Six Hearts,
                        Card Two Hearts,
                        Card Four Hearts,
                        Card Ten Spades,
                        Card Jack Hearts]
            flush hand == Nothing
    describe "straightFlush" $ do
        it "should return a Maybe Hand" $ do
            let hand = [Card Ten Hearts,
                        Card Jack Hearts,
                        Card Queen Hearts,
                        Card King Hearts,
                        Card Ace Hearts]
            straightFlush hand == Just (StraightFlush (Card Ace Hearts))
        it "should return Nothing" $ do
            let hand = [Card Six Hearts,
                        Card Two Hearts,
                        Card Four Hearts,
                        Card Ten Spades,
                        Card Jack Hearts]
            straightFlush hand == Nothing