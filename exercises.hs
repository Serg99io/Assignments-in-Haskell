import Data.List
import System.IO


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

