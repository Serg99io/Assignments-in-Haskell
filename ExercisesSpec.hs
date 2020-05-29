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
         
    describe "onetrue" $ do
       it "all false" $ do
         (onetrue [False,False,False]) `shouldBe` False
       it "one true" $ do
         (onetrue [True,False,False]) `shouldBe` True
       it "all true" $ do
         (onetrue [True,True,True]) `shouldBe` True

    describe "cat" $ do
       it "creates appended strings" $ do
         (cat ["I"," like pizza"]) `shouldBe` "I like pizza"
       it "empty" $ do
         (cat []) `shouldBe` ""
           
    describe "infsup1" $ do
       it "give biggest number" $ do
         (infsup1 [1,2,5,1,3] (>)) `shouldBe` 5
       it "smallest number" $ do
         (infsup1 [1,2,3,4,5,1,4] (<)) `shouldBe` 1

    describe "occurs" $ do
       it "check a" $ do
         (occurs "a" ["c", "a", "b"]) `shouldBe` Just ["b"]
       it "a not in list" $ do
         (occurs "a" ["b","c","d"]) `shouldBe` Nothing
       it "empty" $ do
         (occurs "a" []) `shouldBe` Nothing
         
    describe "tabulate" $ do
       it "adding 5" $ do
         (tabulate (+ 5) 3) `shouldBe` [8,7,6,5]
       it "times 3" $ do
         (tabulate (* 3) 4) `shouldBe` [12,9,6,3,0]

    describe "fold1" $ do
       it "one element" $ do
         (fold1 [2] (+)) `shouldBe` 2
       it "sum" $ do
         (fold1 [2,13,1] (+)) `shouldBe` 16
       it "product" $ do
         (fold1 [1,2,5,2] (*)) `shouldBe` 20
    
    describe "findname" $ do
       it "emptylist" $ do
         (findname "lalaw" []) `shouldBe` False
       it "string not prefix in list" $ do
         (findname "ik" ["pizza", "nein"]) `shouldBe` False
       it "input string in list" $ do
         (findname "ban" ["bank", "pizza"]) `shouldBe` True

    describe "buildlist" $ do
       it "adding 4" $ do
         (buildlist 3 (+ 4)) `shouldBe` [4,5,6,7]
       it "multiplying by 2" $ do
         (buildlist 3 (* 2)) `shouldBe` [0,2,4,6]
         
    describe "filter'" $ do
       it "check for num smaller than 12" $ do
         (filter' (< 12)[13,11,10,8]) `shouldBe` [11,10,8]

    describe "sort'" $ do
       it "ascending" $ do
         (sort' [2,1,4,5] (>)) `shouldBe` [1,2,4,5]
       it "descending" $ do
         (sort' [2,1,4,5] (<)) `shouldBe` [5,4,2,1]
    
    describe "map'" $ do
       it "add 2" $ do
         (map' (+ 2) [1,4,5]) `shouldBe` [3,6,7]
       it "multiplying by 2" $ do
         (map' (* 2) [1,4,5]) `shouldBe` [2,8,10]
         
    describe "andmap'" $ do
       it "all are bigger than 2" $ do
         (andmap' (> 2)[3,4,5,7,8]) `shouldBe` True
       it "one less than three" $ do
         (andmap' (> 3) [2,5,6,4]) `shouldBe` False
   
    describe "ormap'" $ do
       it "one is true" $ do
         (ormap' (> 4) [6,1,2,3]) `shouldBe` True
       it "none are true" $ do
         (ormap' (> 20) [1,2,3,4,5]) `shouldBe` False

