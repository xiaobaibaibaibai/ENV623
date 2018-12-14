

cycle :: [a] - > a
cycle [] = error "empty list"
cycle xs = ys where ys = xs ++ ys


main = do
    print (cycle "abc")