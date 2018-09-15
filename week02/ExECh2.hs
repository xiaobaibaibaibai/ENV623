-- Lazy Susan's first 

first p xs =    if null ys then Nothing
                else Just (head ys)
                where ys = filter p xs                
