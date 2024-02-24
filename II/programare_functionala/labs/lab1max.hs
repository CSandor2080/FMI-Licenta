maxim :: Int -> Int -> Int     --max a doua elemente if cu identare
maxim x y =  
    if x>y 
        then x 
        else y

maxim1 :: Int -> Int -> Int -> Int  -- max a 3 elemente folosind maxim
maxim1 x y z = maxim x (maxim y z)

maxim2 :: Int -> Int -> Int -> Int  -- max a 3 elemente folosind let..in..
maxim2 x y z = 
    let 
        u=maxim x y 
    in 
        maxim u z 

--maxim2 x y z = if x > maxim y z then x else if y>z then y else z    --max a 3 elem cu if si max 
{-maxim2 x y z =                                                      --identare
    if x > maxim y z 
        then x 
        else 
            if y>z 
                then y 
                else z  -}