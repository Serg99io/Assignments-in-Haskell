module ExercisesSpec where

import Test.Hspec
import Exercises

spec :: Spec
spec = do
    describe "sum'" $ do
       it "twenty" $ do
         (sum' [7,8,5]) `shouldBe` 20
       	  