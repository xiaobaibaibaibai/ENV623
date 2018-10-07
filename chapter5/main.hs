import Data.List (nub, sort)  

add :: Num a => a -> [a]
add x = x : ([[]]!!0)


type Matrix a = [Row a]
type Row a = [a]
type Grid = Matrix Digit
type Digit = Char


digits :: [Digit]
digits = ['1' .. '9']

blank :: Digit -> Bool
blank = (== '0')


solve :: Grid -> [Grid]
solve = filter valid . completions

completions :: Grid -> [Grid]
completions = expand . choices


expand :: Matrix [Digit] -> [Grid]
expand = cp . map cp

choices :: Grid -> Matrix [Digit]
choices = map (map choice)
choice d = if blank d then digits else [d]


cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
              where yss = cp xss


valid :: Grid -> Bool
valid g = all nodups (rows g) &&
          all nodups (cols g) &&
          all nodups (boxs g)

nodups :: (Eq a) => [a] -> Bool
nodups [] = True
nodups (x:xs) = all (/=x) xs && nodups xs


rows :: Matrix a -> Matrix a
rows = id


boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup .
       map cols .
       group . map group

group :: [a] -> [[a]]
group [] = []
group xs = take 3 xs:group (drop 3 xs)

ungroup :: [[a]] -> [a]
ungroup = concat


-- pruneRow :: Num a => [[a]] ->[[a]] 
pruneRow row = map (remove fixed) row
               where fixed = [d | [d] <- row]


-- remove :: (Eq a) => [a] -> [a] -> [a]
remove ds [x] = [x]
remove ds xs = filter (`notElem` ds) xs


-- evenTest :: (Integral a) => [a] -> [a]
evenTest xs = filter even xs

-- notTest :: (Eq a) => a -> [a] -> [a]
notTest x xs = if notElem x xs
               then xs
               else [x]


-- [[6],[3,6],[3],[1,3,4],[4]]

-- cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)


-- [1, 2, 3] : cols [[4, 5, 6], [7, 8, 9]]
-- [1, 2, 3] : [4, 5, 6] : cols [[7, 8, 9]]
-- [1, 2, 3] : [4, 5, 6] : [[7],[8],[9]]

transpose :: [[a]] -> [[a]]
transpose [[]] = []
transpose xss = map head xss:transpose (map tail xss)


nodups1 :: (Ord a) => [a] -> Bool
nodups1 xs = and (zipWith (/=) ys (tail ys) )
            where ys = sort xs

nub1 :: (Ord a) => [a] -> [a]
nub1 [] = []
nub1 (x:xs) = x : nub1 (filter (/= x) xs)  

takeWhile1 :: (Eq a) => (a -> Bool) -> [a] -> [a]
takeWhile1 p [] = []
takeWhile1 p (x:xs) = if p x
                      then x : (takeWhile p xs)
                      else []

dropWhile1 :: (Eq a) => (a -> Bool) -> [a] -> [a]
dropWhile1 p [] = []
dropWhile1 p (x:xs) = if p x
                      then dropWhile1 p xs
                      else x:xs


minimum1 :: (Ord a) => [a] -> a
minimum1 [x] = x
minimum1 (x:xs) = if x >= head xs
                  then minimum1 xs
                  else minimum1 (x:(tail xs))


main = do
    -- print (cols [[1, 2, 3], [4, 5, 6], [7, 8, 9]])
    -- print (cols [[7, 8, 9]])
    -- print (transpose [[1, 2, 3], [4, 5, 6], [7, 8, 9]])
    -- print (transpose [[7, 8, 9]])
    -- print (nodups1 [4, 2, 6, 4, 7])
    -- print (nub1 [2, 4, 1, 8, 2, 8])

    -- print (span (> 3) [1..5])
    -- print (span (< 3) [1..5])
    -- print (span (3 >) [1..5])
    -- print (span (3 <) [1..5])

    -- print (takeWhile1 (< 3) [1..5])
    -- print (dropWhile1 (< 3) [1..5])

    print (minimum1 [1..3])
    print (minimum1 [3, 2, 1])
    print (minimum1 [12, 4, 7, 9, 11])
    