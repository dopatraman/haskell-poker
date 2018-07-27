module Data.Hand.UnigraphSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Hand.Unigraph

spec :: Spec
spec = do
    describe "comboGraph" $ do
        it "constructs a combo graph" $ do
            let xs = [1,2,3]
            let combos = comboGraph xs 3
            head combos == (Node 1 [(Node 2 [(Node 3 [])]), (Node 3 [])])
    describe "getAllPaths" $ do
        it "gets all paths" $ do
            let u = (Node 1 [(Node 2 [(Node 3 [])]), (Node 3 [])])
            getAllPaths u == [[1,2,3], [1,3]]
    describe "combos" $ do
        it "gets combos" $ do
            (combos [1,2,3] 3) == [[1,2,3]]