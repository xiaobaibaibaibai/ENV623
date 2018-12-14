import Data.List
import Data.Char

diagonalMatrix = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 16]]

--question1
thatD [a] = a
thatD m = map helper1 (zip [0..(length m-1)] m)

helper1 :: (Int, [Int]) -> Int  --- if a paire (Int, [Int])
helper1 (i, xs) = head $ drop i xs


--question7
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent n m p = and [n /= m, n/= p, m /= p] -- and flow list


--question8
ordPairs :: Int -> [(Int, Int)]
ordPairs n = [(x, y) | y <- [2..n-1], x <- [1..y-1]]


--question10
data Name = Short String | Fullname String String

title :: Name -> String
title (Fullname firstName lastName) = "Esteemed " ++ firstName ++ " " ++ lastName
title (Short nickName) = nickName


-- question13(a)
t13 :: [a] -> [a]
t13 [] = []
t13 xs = concat $ zipWith (\x y -> if mod x 2 == 1 then [y] else replicate 2 y ) [1..(length xs)] xs

-- question13(b)
u13 :: [a] -> [a]
u13 [] = []
u13 xs = u13Help 1 xs

u13Help :: Int -> [a] -> [a]
u13Help _ [] = []
u13Help n (x:xs) = if mod n 2 == 1
                   then x : (u13Help (n+1) xs)
                   else (replicate 2 x) ++ (u13Help (n+1) xs)




--question14(a)
f14 :: [Int] -> [Int]
f14 xs = f14Help $ zip xs $ tail xs 

f14Help :: [(Int, Int)] -> [Int]
f14Help xs = [ b-a | (a, b) <- xs, a < b]

--question14(a)
g14 :: [Int] -> [Int]
g14 [] = []
g14 [a] = []
g14 (x:y:ys) = if x >= y
               then g14 (y:ys)
               else (y-x) : g14(y:ys)


-- question15(a)
f15 :: [Int] -> [String]
f15 [] = []
f15 [a] = []
f15 xs = f15Help (zip xs (tail xs))

f15Help :: [(Int ,Int)] -> [String]
f15Help xs = [ if a > b then ">" else "<" | (a, b) <- xs, a /= b]

-- question15(a)
g15 :: [Int] -> [String]
g15 [] = []
g15 [a] = []
g15 (x:y:ys) = if x == y
              then g15 (y:ys)
              else if x < y
                   then "<" : g15 (y:ys)
                   else ">" : g15 (y:ys)


-- question16
allRec :: (a -> Bool) -> [a] -> Bool
allRec f [] = True
allRec f (x:xs) = (&&) (f x) (allRec f xs)

allFold :: (a -> Bool) -> [a] -> Bool
allFold f xs = foldr ((&&) . f) True xs



-- question17
sublist :: (Eq a) => [a] -> [a] -> Bool
sublist [a] [] = False
sublist (x:xs) [] = False
sublist [] _ = True
sublist (x:xs) (y:ys) = if x == y
                        then sublist xs ys
                        else sublist (x:xs) ys


-- question20
-- printName :: IO String
printName = do {
    putStrLn "Who are you";
    xs <- getLine;
    putStrLn ("Oh, hello " ++ xs)
}


--some function

--diff [4, 2, 7, 3, 6, 5] = [4-2, 2-7, 7-3, 3-6, 6-5]
diff :: [Int] -> [Int]
diff [] = []
diff xs = zipWith (-) xs (tail xs)





main = do
    -- print (thatD diagonalMatrix)
    -- print (threeDifferent 1 2 3)
    -- print (ordPairs 5)
    -- print (title (Fullname "Irwin" "Cohen"))
    -- print (title (Short "Irwin"))
    -- print (allRec even [2, 4, 6])
    -- print (allRec even [2, 5, 6])
    -- print (allRec even [])
    -- print (allFold even [2, 4, 6])
    -- print (allFold even [2, 5, 6])
    -- print (allFold even [])
    -- print (sublist [1, 5, 3, 1] [1, 2, 5, 1, 3, 1])
    -- print (sublist [1, 3, 5, 1] [1, 2, 5, 1, 3, 1])
    -- print (sublist [1, 2, 3, 4] [1, 2, 5, 1, 3, 1])
    -- printName
    -- print (f15 [4, 2, 5, 6, 1, 8])
    -- print (f15 [])
    -- print (f15 [3])
    -- print (f15 [3, 3, 1, -3])
    -- print (g15 [4, 2, 5, 6, 1, 8])
    -- print (g15 [])
    -- print (g15 [3])
    -- print (g15 [3, 3, 1, -3])
    -- print (f14 [4, 2, 5, 6, 1, 8])
    -- print (f14 [])
    -- print (f14 [3])
    -- print (f14 [3, 3, 1, -3])
    -- print (g14 [4, 2, 5, 6, 1, 8])
    -- print (g14 [])
    -- print (g14 [3])
    -- print (g14 [3, 3, 1, -3])
    -- print (u13 "abcd")
    -- print (u13 "abcde")
    -- print (u13 [1,2,3,4])
    -- print (u13 [1,2,3,4,5])
    -- print (u13 "")
    -- print (t13 "abcd")
    -- print (t13 "abcde")
    -- print (t13 [1,2,3,4])
    -- print (t13 [1,2,3,4,5])
    -- print (t13 "")




    -- print (diff [4, 2, 7, 3, 6, 5])
