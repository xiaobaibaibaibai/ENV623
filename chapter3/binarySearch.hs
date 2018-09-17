
until1 :: (a -> Bool) -> (a -> a) -> a -> a
until1 p f x = if p x then x else until1 p f (f x)

type Interval = (Integer, Integer)

shrink :: Float -> Interval -> Interval
shrink x (l, r) = if midpoint `leq` x 
                  then (midpoint, r)
                  else (l, midpoint)
                  where midpoint = choose (l, r)
                        midpoint `leq` x = fromInteger midpoint <= x

choose :: Interval -> Integer
choose (l, r) = (l + r) `div` 2

bound :: Float -> Interval
bound x = (lower x, upper x)

lower :: Float -> Integer
lower x = until1 (`leq` x) (*2) (-1)
        where m `leq` x = fromInteger m <= x

upper :: Float -> Integer
upper x = until1 (x `lt`) (*2) 1
          where x `lt` n = x < fromInteger n
                m `leq` x = fromInteger m <= x


floor1 :: Float -> Integer
floor1 x = fst (until1 unit (shrink x) (bound x))
          where unit (l,r) = (l+1 == r)

bound2 :: Float -> Interval
bound2 x = (0, until above (*2) 1)
          where above n = x `lt` (n*n)
                x `lt` n = x < fromInteger n

shrink2 :: Float -> Interval -> Interval
shrink2 x (l, r) = if midpoint `leq` x 
                  then (midpoint, r)
                  else (l, midpoint)
                  where midpoint = (l + r) `div` 2
                        midpoint `leq` x = (fromInteger midpoint)^2  <= x

isqrt :: Float -> Integer
isqrt x = fst (until1 unit (shrink2 x) (bound2 x))
          where unit (l, r) = (l + 1 == r)

main = do
    print(isqrt 0.16)