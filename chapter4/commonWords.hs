



span :: (a -> Bool) -> [a] -> ([a],[a])
span p []     = ([],[])
span p (x:xs) = if p x then (x:ys,zs)
                else ([],x:xs)
                where (ys,zs) = span p xs


-- commonWords :: Int -> [Char] -> [Char]
-- commonWords n = concat . map showRun . take n .
--                 sortRuns . countRuns . sortWords .
--                 words . map toLower

main = do
    print(span (<4) [1..10])
