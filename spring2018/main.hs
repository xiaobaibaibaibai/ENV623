import Data.Char
import Data.List

degenerate :: Float -> Float -> Float -> Bool
degenerate a b c = res >= 0
                   where res = b^2 - (4*a*c)


capitalise :: String -> String
capitalise [] = ""
capitalise (x:xs) = toUpper x : (map toLower xs)

title :: [String] -> [String]
title [] = [""]
title (w:words) = capitalise w : [ if length x >= 4 then capitalise x else map toLower x | x <- words]


searchRec :: String -> Char -> [Int]
searchRec str goal = if null str then [] else helper (zip str [0..length str - 1]) goal

helper :: [(Char, Int)] -> Char -> [Int]
helper (w:ws) goal = if null ws then snd w : [] 
                     else if fst w == goal then snd w : helper ws goal
                     else helper ws goal


main = do
    -- print(searchRec "senselessness's" 's')
    print (""++[])
    print ("":[])
    print ([]:[[1,2],[3,4]])
    print ([]++[[1,2],[3,4]])
    print ([[1,2],[3,4]]:[])
    print ([[1,2],[3,4]]++[])