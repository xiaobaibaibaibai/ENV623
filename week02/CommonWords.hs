-- Recall that modules are capitalized
module CommonWords (commonWords) where

-- Let's copy-paste from the commonWords
-- example from last time
import Prelude hiding (Word,span)
import qualified Prelude as P

-- Loading just one function from a particular module 
import Data.Char (toLower)

-- Loading another function from another module
import Data.List (sort)

-- Declaring type synonyms
type Text = [Char]
type Word = [Char]

-- Defining showRun
showRun :: (Int,Word) -> [Char]
showRun (n,w)   =   w ++ ": " ++ show n ++ "\n"

-- Defining sortWords, since that's what we're calling it.
sortWords :: [Word] -> [Word]
sortWords =  sort 

-- Defining sortRuns with the same ease
-- Why are we reversing a sorted list?
sortRuns :: [(Int,Word)] -> [(Int,Word)]
sortRuns = reverse . sort

-- Skipping ahead, you can find this definition in Ch. 4
countRuns :: [Word] -> [(Int,Word)]
countRuns []    =   []
countRuns (w:ws)=   (1+length us,w):countRuns vs
                    where (us,vs) = span (==w) ws

{-
 Note that this definition is coming after that
    of a function that calls it
-}
span :: (a -> Bool) -> [a] -> ([a],[a])
span p []       =   ([],[])
span p (x:xs)   =   if p x then (x:ys,zs)
                    else([],x:xs)
                    where (ys,zs) = span p xs 


-- Declaring our main function
commonWords :: Int -> Text -> String
commonWords n   =   concat . map showRun . take n .
                    sortRuns . countRuns . sortWords .
                    words . map toLower

