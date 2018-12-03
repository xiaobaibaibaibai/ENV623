import Data.List

-- question 1
sort1 [] = []
sort1 (x:xs) = insert1 x (sort1 xs)

insert1 x [] = [x]
insert1 x (y:ys) = if x <= y 
                  then x:y:ys 
                  else y:insert1 x ys

-- question 2
length1 xs = foldl' (\n x -> n + 1) 0 xs


