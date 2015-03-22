type MSet a = [(a,Int)]

add :: (Ord a) => a -> (MSet a) -> MSet a
add a [] = [(a,1)]
add a ((x,y):xs) = if a == x
                   then (x,y+1) : add a xs
                  else add a xs

toMSet :: (Ord a) => [a] -> MSet a
toMSet [] =[]
toMSet (x:xs) =let a = ((x, 1+conta x xs) :toMSet xs)
               in repetidos a 

conta ::(Eq a)=> a->[a]->Int
conta a [] = 0
conta a (x:xs) = if a ==x
                 then 1 + conta a xs
                 else conta a xs

repetidos ::(Eq a)=> MSet a->MSet a 
repetidos [] =[]
repetidos ((x,a):xs) = (x,a):repetidos (filter (\(t,y) -> t/=x) xs)



moda :: MSet a -> a
moda l = fst (last (ordena l))


ordena :: MSet a->MSet a
ordena [] =[]
ordena (x:xs) = inser x (ordena xs)

inser :: (a,Int)->MSet a ->MSet a
inser (a,b) [] =[(a,b)]
inser (a,b) ((a1,b1):xs)= if b1>b
                          then (a,b):(a1,b1):xs
                          else (a1,b1):inser (a,b) xs



mUnion :: (Ord a) => (MSet a) -> (MSet a) -> (MSet a)
mUnion [] []=[]
mUnion [] l=l
mUnion m [] =m 

mUnion ((x,y):xs) l  =let a = filter (\((x1,y1))-> x==x1) l 
                      in repetidos ((x,y+sum (map (snd) a)):mUnion xs l)
 
                               
