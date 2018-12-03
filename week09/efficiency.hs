-- Local vs Global
foo1 n = sum ( take n primes )
         where
         primes     = [x | x <- [2..], divisors x == [x]] 
         divisors x = [d | d <- [2..x], x `mod` d == 0] 


foo2 n = sum (take n primes) 
primes = [x | x <- [2..], divisors x == [x]] 
divisors x = [d | d <- [2..x], x `mod` d == 0]

foo3 = \n -> sum ( take n primes )
       where
       primes     = [x | x <- [2..], divisors x == [x]] 
       divisors x = [d | d <- [2..x], x `mod` d == 0]

-- The mean

{--
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)
--}

foldl' f e []       = e
foldl' f e (x:xs)   = y `seq` foldl' f y xs
                      where y = f e x

sumlen = foldl' f (0,0)
         where f (s,n) x = s `seq` n `seq` (s+x,n+1)

mean [] = 0
mean xs = s / fromIntegral n
          where (s,n) = sumlen xs

-- cp
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]

cp' :: [[Int]] -> [[Int]]
cp' = foldr op [[]] where op xs yss = [x:ys | x <- xs, ys <- yss]

