--- Bird Chapter 1, Exercise G

{-
Let's look at Bird's solution.
-}


--- Giving the type signature for our function, "song"
song :: Int -> String

{-
We'll define "song" in a top-down way. Note that
this solution uses recursion and also breaks the
task down into smaller pieces.
-}

song n  =   if n==0 then ""
            else song (n-1) ++ "\n" ++ verse n
verse n =   line1 n ++ line2 n ++ line3 n ++ line4 n

line1 n =   if n==1 then
            "One man went to mow\n"
            else
            numbers!!(n-1) ++ " men went to mow\n"

line2 n =   "Went to mow a meadow\n"

line3 n =   if n==1 then
            "One man and his dog\n"
            else
            numbers!!(n-2) ++ " men, " ++ count (n-2)
                ++ "one man and his dog\n"

line4 n =   "Went to mow a meadow\n\n"

count n =   if n==0 then ""
            else
            numbs!!(n-1) ++ " men, " ++ count (n-1)

--- Don't forget this handy lists!

numbers =   ["Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine"]
numbs =     ["two", "three", "four", "five", "six", "seven", "eight"]
              

main = do
    putStrLn (song 3)