type MSet a = [(a,Int)]

union :: Ord a => MSet a -> MSet a -> MSet a 
union l [] = l
union [] g = g
union l g = let a = l++g
            in verifica (ordena a)

ordena ::Ord a =>MSet a ->MSet a
ordena [] = []
ordena ((x,y):xs) = inser (x,y) (ordena xs)

inser :: Ord a=> (a,Int)->MSet a->MSet a
inser (x,y) [] = [(x,y)]
inser (x,y) ((d,g):xs) = if d>x
                        then (x,y):(d,g):xs
                        else (d,g):inser (x,y) xs
verifica :: Eq a =>MSet a->MSet a
verifica [] =[]
verifica [(x,y)]= [(x,y)]
verifica ((x,y):i@(z,u):xs)= if x==z
                           then (x,y+u):verifica xs
                            else (x,y):verifica (i:xs)
