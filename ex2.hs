maximos :: [Int]->Int
maximos [x]= x
maximos (x:xs)= max x (maximos xs)

maxi :: [Int]->Int

maxi [x]= x
maxi l = head(reverse(ordena l))

ordena ::[Int]->[Int]
ordena [] = []

ordena (x:xs) = inser x (ordena xs)

inser ::Int->[Int]->[Int]
inser x[]= [x]
inser x (y:ys) = if x<y
                 then x:y:ys
                 else y:inser x ys
