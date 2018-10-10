import Data.List


intPart :: String -> Integer
intPart str = read (takeWhile (/= '.') str) :: Integer

fractionalPart :: String -> Float
fractionalPart str = read ('0': dropWhile (/= '.') str) :: Float

segments1 =  concat . map inits . tails
segments2 =  concat . map tails . inits


mss :: [Int] -> Int
mss = maximum . map sum . segments1


cp = foldr op [[]]
     where op xs xss = [x:ys | x <- xs, ys <- xss]

main = do
    -- print (intPart "123.456");
    -- print (fractionalPart "123.456");

    {-
    print (tails "abc")
    print (inits "abc")
    print (map inits (tails "abc"))
    print (segments1 "abc")
    print (segments2 "abc")
    print (mss [-1,2,-3,5,-2,1,3,-2,-2,-3,6])
    -}
    
    {-
    print (cp [[1], [2], [3]])
    print (cp [[1, 2], [3, 4], [5, 6]])
    print (cp [[1, 2], [], [5, 6]])
    -}

    print (length (cp [[1], [2, 3, 4], [5, 6]]))
    print (map length [[1], [2, 3, 4], [5, 6]])
    print (product [[2, 3, 4], [5, 6]])
    