--- Bird Chapter 1, Exercise B

--- Note that sin is part of the prelude module,
--- so we don't need to load anything in order to
--- use this function.

theta = 2.77

--- Testing on one value is not enough;
--- we're doing this just to demonstrate
--- what happens in one case.

fun1 :: Float -> Float
fun1 x = (sin x) ^ 2

-- `div` can only be used for Integer
fun2 :: Float -> Float
fun2 a = sin (2*a) / (2*pi)

main = do
    print (fun2 theta)
