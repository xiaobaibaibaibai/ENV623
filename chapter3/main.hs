import Data.Char (toLower,isAlpha)

expp :: (Fractional a, Integral b) => a -> b -> a
expp x n | n == 0 = 1
         | n == 1 = x
         | n < 0 = 1 / (expp x (-n))
         | even n = expp (x*x) m
         | odd n = x * expp (x*x) m
         where m = n `div` 2

floor1 :: Float -> Integer
floor1 = read . takeWhile (/= '.') . show


until1 :: (a -> Bool) -> (a -> a) -> a -> a
until1 p f x = if p x then x else until1 p f (f x)

floor2 x = if x < 0
          then until1 (`leq` x) (subtract 1) (-1)
          else until1 (x `lt`) (+1) 1 - 1
          where m `leq` x = fromInteger m <= x
                x `lt` n = x < fromInteger n


main = do
    print(floor2 (negate 4.3))
    print(floor2 3.2)
