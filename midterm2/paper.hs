x = 4
ys = [1, 2]
y = 3
main = do
    let out = x : reverse (ys ++ [y])
    print ( out )