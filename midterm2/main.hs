type Matrix a = [Row a]
type Row a = [a]
type Vector a = [a]


transpose :: [[a]] -> [[a]]
transpose [xs] = [[x] | x <- xs]
transpose (xs:xss) = zipWith (:) xs (transpose xss)

scalarMult :: Num a => [a] -> [a] -> a
scalarMult xs ys = sum (zipWith (*) xs ys)

matMult :: Num a => Matrix a -> Matrix a -> Matrix a
matMult ma mb = [map (scalarMult row) mbt | row <- ma]
                where mbt = transpose mb

vectorScalar :: Num a => [a] -> a -> [a]
vectorScalar xs y = map (* y) xs 

matrixVector :: Num a => Matrix a -> [a] -> [a]
matrixVector m v = [ scalarMult row v | row <- m]

isEigenvector :: (Eq a, Num a) => Matrix a -> Vector a -> a -> Bool
isEigenvector m v s = mv == vs
                      where mv = matrixVector m v
                            vs = vectorScalar v s


insertOne :: Num a => Int -> Int -> [a]
insertOne i n = take (i - 1) (repeat 0) ++ [1] ++ take (n - i) (repeat 0)

idMatrix :: Num a => Int -> [[a]]
idMatrix n = [insertOne i n | i <- [1..5]]

fillOne :: Num a => [a] -> [a]
fillOne [x] = []
fillOne (x:xs) = [1] ++ (fillOne xs) 


matrixIJ :: Num a => Int -> Int -> Matrix a -> Matrix a
matrixIJ i j m = prev ++ (fillOne target : []) ++ remain
                 where prev = take (i - 1) m
                       remain = drop i m
                       target = head (drop (i - 1) m)


build1 :: Int -> Int -> Int -> Matrix Int
build1 row col l = [[ if((x==col) && (y==row)) then 1 else 0 | x <- [1..l]] | y <- [1..l]]


saddlePoint :: Matrix -> Bool
saddlePoint 



main = do
    -- print (transpose [[1, 2, 3], [4, 5, 6], [7, 8, 9]])
    -- print (transpose [[7, 8, 9]])
    -- print (transpose [[5, 6], [7, 8]])
    -- print (matMult [[1, 2], [3, 4]] [[5, 6], [7, 8]])
    -- print (matrixVector [[1, 2, 3], [4, 5, 6], [7, 8, 9]] [2, 1, 3] )
    -- print (isEigenvector [[1, 2], [3, 4]] [1, 2] 3)
    -- print (isEigenvector [[0, 1], [-2, -3]] [1, -1] (-1))

    -- print (insertOne 0 5)
    -- print (idMatrix 5)

    -- print (fillOne [0, 0, 0, 0])
    -- print ( matrixIJ 3 5
    --     [
    --     [0, 0, 0, 0, 0, 0],
    --     [0, 0, 0, 0, 0, 0],
    --     [0, 0, 0, 0, 0, 0],
    --     [0, 0, 0, 0, 0, 0]
    --     ])
    print (build1 3 4 6)


