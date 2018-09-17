import Data.Char (toLower,isAlpha)

add :: Num a => a -> a -> a
add x y = x + y

double :: Num a => a -> a
double x = 2 * x

suffix :: Integer -> String
suffix d | d == 1 || d == 21 || d == 31 = "st" 
         | d == 2 || d == 22            = "nd" 
         | d == 3 || d == 23            = "rd" 
         | otherwise                    = "th"

getDigit :: Char -> Int
getDigit c = read [c]

addSum :: String -> String
addSum cin = cin ++ show (n `div` 10) ++ show (n `mod` 10)
             where n = sum (map getDigit cin)



isPalindrome :: String -> Bool
isPalindrome xs = xy == reverse xy
                where xy = map toLower (filter isAlpha xs)



expp :: Integer -> Integer -> Integer
expp x n | n == 0 = 1
        | n == 1 = x
        | even n = expp (x*x) m
        | odd n = x * expp (x*x) m -- here the code on book is wrong 
        where m = n `div` 2


main = do
    print (expp 2 5)
    
