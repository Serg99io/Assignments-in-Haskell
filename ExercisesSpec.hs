module ExercisesSpec where

import Test.Hspec
import Exercises

spec :: Spec
spec = do
    describe "sum'" $ do
       it "twenty" $ do
         (sum' [7,8,5]) `shouldBe` 20

    describe "alltrue" $ do
       it "all false" $ do
         (alltrue [False,False,False]) `shouldBe` False
       it "one true" $ do
         (alltrue [True,False,False]) `shouldBe` False
       it "all true" $ do
         (alltrue [True,True,True]) `shouldBe` True
