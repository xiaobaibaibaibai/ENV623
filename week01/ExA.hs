--- Bird Chapter 1, Exercise A

--- Let's declare the function and test it direction


import Data.List
double :: Integer -> Integer
double x = 2*x

--- Let's give special names to the functions in question
--- Note that we give type signatures for each.
--- Try to do it without them!

summapDouble :: [Integer] -> Integer
summapDouble = sum . map double

doubleSum :: [Integer] -> Integer
doubleSum = double . sum

summapSum :: [[Integer]] -> Integer
summapSum = sum . map sum

sumConcat :: [[Integer]] -> Integer
sumConcat = sum . concat

sumSort :: [Integer] -> Integer
sumSort = sum . sort

--- We can don't know how to prove the claimed identities
--- They follow from these simpler rules
