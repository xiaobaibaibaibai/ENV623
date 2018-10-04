solve = search . choices
search cm
    | not (safe pm) = []
    | complete pm   = [extract pm]
    | otherwise     = concat (map search (expand1 pm))
    where pm = prune cm

complete = all (all single)

safe cm  = all ok (rows cm) &&
           all ok (cols cm) &&
           all ok (boxs cm)

ok row = nodups [x | [x] <- row]            

extract = map (map head)

expand1 rows
    =   [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs]
        where
        (rows1,row:rows2) = break (any smallest) rows
        (row1,cs:row2)    = break smallest row
        smallest cs       = length cs == n
        n                 = minimum (counts rows)

counts = filter (/=1) . map length . concat

single [_] = True
single _   = False

prune = pruneBy boxs . pruneBy cols . pruneBy rows

pruneBy f = f . map pruneRow . f

remove ds [x] = [x]
remove ds xs  = filter (`notElem` ds) xs

pruneRow row = map (remove fixed) row where fixed = [d | [d] <-row]

ungroup = concat

group [] = []
group xs = take 3 xs:group (drop 3 xs)

boxs = map ungroup . ungroup .
       map cols .
       group . map group


cols [xs]       = [[x] | x <- xs]
cols (xs:xss)   = zipWith (:) xs (cols xss)


rows = id

nodups []       = True
nodups (x:xs)   = all (/=x) xs && nodups xs

valid g     = all nodups (rows g) &&
              all nodups (cols g) &&
              all nodups (boxs g)

expand = cp . map cp

cp []       = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
              where yss = cp xss

choices  = map (map choice)
choice d = if blank d then digits else [d]

blank = (== '0')

digits = ['1'..'9']

myGrid = [['5','3','0','0','7','0','0','0','0'],
          ['6','0','0','1','9','5','0','0','0'],
          ['0','9','8','0','0','0','0','6','0'],
          ['8','0','0','0','6','0','0','0','3'],
          ['4','0','0','8','0','3','0','0','1'],
          ['7','0','0','0','2','0','0','0','6'],
          ['0','6','0','0','0','0','2','8','0'],
          ['0','0','0','4','1','9','0','0','5'],
          ['0','0','0','0','8','0','0','7','9']]
         
