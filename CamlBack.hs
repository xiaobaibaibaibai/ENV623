import Data.Char

discount :: Int -> Int
discount x = round (0.9 * (fromIntegral x))

pennypincher :: [Int] -> Int
pennypincher prices = sum [x | x <- (map discount prices), x <= 19900]

-- [4500, 19900, 22000, 39900]

pennypincherRec :: [Int] -> Int
pennypincherRec [] = 0
pennypincherRec (price:prices) = if x <= 19900 then x + pennypincherRec prices
                                 else pennypincherRec prices
                                 where x = discount price

-- multDigits :: String -> [Char]
-- multDigits str = [x | x <- str, x == 'a']

multDigits :: String -> [Int]
multDigits str = [digitToInt x | x <- str, isDigit x]


-- capitalise :: String -> String
-- capitalise str = [toUpper x | x <- (take 1 str)] : [toLower x | x <- (tail str)]

capitalise :: String -> String
capitalise str = [toUpper x | x <- (take 1 str)] ++ [toLower x | x <- (tail str), not (null str)]

-- capitalise :: String -> String
-- capitalise str = toUpper (head str) : [toLower x | x <- (tail str)]


myTake :: Int -> [a] -> [a]
myTake k _ | k<=0      = []
myTake _ []            = []
myTake k (x:xs)        = x:myTake (k-1) xs


main = do
    print(myTake 3 [1,2,3,4,5,6])
    -- print(discount (sum [4500, 19900, 22000]))
    -- print(pennypincher [4500, 19900, 22000, 39900])
    -- print(pennypincherRec [4500, 19900, 22000, 39900])