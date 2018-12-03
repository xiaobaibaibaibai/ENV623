

repeat1 x = x:repeat1 x

repeat2 x = xs
            where xs = x:xs

main = do 
    print (last $ take 10000000 $ repeat1 1 )
    print (last $ take 10000000 $ repeat2 1 )