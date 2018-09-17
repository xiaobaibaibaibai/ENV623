
type List = [Integer]

createList :: Integer -> List
createList x = [x | x <- [1..x]]

divisors :: Integer -> List
divisors x = [d| d <- [2..x-1], x `mod` d == 0]

position :: (Eq a) => a -> [a] -> Int
position x xs
    = head ([j | (j,y) <- zip [0..] xs, y==x] ++ [-1])

main = do
    -- print(map (+1) [1,2,3])
    print(position 1 [1..10])
