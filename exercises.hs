module Exercises where


import Data.List

--ex 138 sum-list

-- the sum function which consumes a list of numbers and computes the sum
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

--ex 140 alltrue & onetrue

--the alltrue function which consumes a list of Boolean values and sees if all of them are true
--an empty list will return True
alltrue :: [Bool] -> Bool
alltrue [] = True
alltrue (x:xs) = x && alltrue xs

--the onetrue function which consums a list of Boolean and if one of them is true, returns true

onetrue :: [Bool] -> Bool
onetrue [] = False
onetrue (x:xs) = x || onetrue xs

--ex 141 cat

--the function cat consumes a list of string and appends them all into one long string

cat :: [String] -> String
cat [] = ""
cat (x:xs) = x ++ cat xs

--ex 238 inf & sup

--the function infsup1 will take a list of numbers and a compare function as input
--and will output the result. > will give the biggest number in the list

infsup1 :: Num a => [a] -> (a -> a -> Bool) -> a
infsup1 [x] func = x
infsup1 (x:xs) func
        | func x (infsup1 xs func) = x
        | otherwise = infsup1 xs func

--ex 242 occurs

--the function occurs will take a string S and list of strings as input
-- and will output the remainder of the list after string S, and Nothing if it doesnt occur

occurs :: String -> [String] -> (Maybe [String])
occurs str [] = Nothing
occurs str (x:xs)
        | str == x = Just xs
        | otherwise = occurs str xs
               

--ex 250 tabulate

-- the function takes a function and number x as input and tabulates the function between x
-- and 0 in a list, The output will be a list

tabulate :: (Num a, Eq a) => (a -> a) -> a -> [a]
tabulate func x
        | x == 0 = [func 0] 
        | otherwise = [func x] ++ tabulate func (x-1)        


--ex 251 fold1
-- the function fold1 takes as input a list of numbers and a function (sum or product) 
-- and will compute the function of the numbers in the list

fold1 :: (Num a, Eq a) => [a] -> (a -> a -> a) -> a
fold1 [] func = 0
fold1 (x:xs) func
       | xs == [] = x
       | otherwise = func x (fold1 xs func)

--ex 271 findname
--The function ormap' is used in this function (the last function in this file)
--The function findname consumes a name and a list of names
-- and outputs whether any of the names on the list are equal or an extension of the given name
--isPrefixOf checks whether one is a prefix of the other (part of Data.List)

findname :: String -> [String] -> Bool
findname inpstr [] = False
findname inpstr xs = ormap' func xs
        where func z = isPrefixOf inpstr z
      


--Figure 95

--The function build-list consumes a function (like * 5) and a number
--and it will construct a list by applying the function to all numbers between 0 and n (n incl.)

buildlist :: (Num a,Eq a) => a -> (a -> a) -> [a]
buildlist n func
        | n == 0 = [func n]
        | otherwise = buildlist (n-1) func ++ [func n]


--The function filter' consumes a list and a condition
-- and will output a list with all the items in inputlist for which the condition holds

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' func (x:xs)
         | func x = [x] ++ (filter' func xs)
         | otherwise = filter' func xs

--The function sort' will consume a list and a comparison
-- and will produce the sorted list accoridng to the comparison

sort' :: Ord a => [a] -> (a ->a-> Bool) -> [a]
sort' [] _ = []
sort' (x:xs) func = (helpins x (sort' xs func) func)

--The helpins helps the sort function insert the inputs correctly

helpins :: (Ord a) => a -> [a] -> (a -> a -> Bool) -> [a]
helpins  x [] _  = [x]
helpins x (z:zs) fun
       | fun x z = [z] ++ helpins x zs fun
       | otherwise = [x] ++ (z:zs)

--The function map' will consume a list and a function
-- it will output a list by applying the function to eac item of the input list

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' func (x:xs) = [func x] ++ map' func xs

--The function andmap' will consume a list and a condition
-- the output will be True/False depending on if the condtion holds for every items in the list

andmap' :: (a -> Bool) -> [a] -> Bool
andmap' _ [] = True
andmap' func (x:xs) = func x && (andmap' func xs)

--The function ormap' will consume a list and a condition
--the output will be True/False depending on if the condition holds for at least one item in list

ormap' :: (a->Bool) -> [a] -> Bool
ormap' _[] = False
ormap' func (x:xs) = func x || (ormap' func xs)