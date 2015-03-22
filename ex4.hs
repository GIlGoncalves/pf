type Ponto = (Float, Float)  -- (Abcissa, Ordenada)
type Circulo = (Ponto, Float) -- (Centro, Raio)


fora :: Ponto -> Circulo -> Bool

fora (x,y) ((z,u),a) = if (dist (x,y) (z,u)) > a
                       then True 
                       else False 

dist :: Ponto->Ponto->Float
dist (x,y) (u,i)= sqrt((x-u)^2 + (y-i)^2)

filtraFora :: Circulo -> [Ponto] -> Int 

filtraFora ((a,b),c) [] = 0
filtraFora ((a,b),c) ((u,m):xs) = if fora (u,m) ((a,b),c) == True
                                  then 1  + filtraFora ((a,b),c) xs
                                  else filtraFora ((a,b),c) xs

dentro :: Ponto -> Circulo -> Bool
dentro (x,y) ((a,b),c) = if (dist (x,y) (a,b)) <c 
                         then True
                         else False

filtraDentro :: Ponto -> [Circulo] -> Int
filtraDentro (x,y) [] = 0
filtraDentro (x,y) (((a,b),c):xs) = if (dentro (x,y) ((a,b),c)) == True 
                                    then 1 + filtraDentro (x,y) xs 
                                    else filtraDentro (x,y)  xs



type Rectangulo = (Ponto,Ponto)

quadrado :: Rectangulo -> Bool
quadrado ((x,y),(z,u)) = if (x==z) && (y==u)
                        then True
                        else False

contaQuadrados :: [Rectangulo] -> Int 
contaQuadrados [] = 0
contaQuadrados (((x,y),(z,u)):xs) = if (quadrado ((x,y),(z,u))) == True
                                   then 1 + contaQuadrados xs
                                   else contaQuadrados xs

roda :: Rectangulo -> Rectangulo 
roda ((a,b),(c,d)) = ((a,b),(d,b))

rodaTudo :: [Rectangulo] -> [Rectangulo] 
rodaTudo [] = []
rodaTudo (((a,b),(c,d)):xs)= roda ((a,b),(c,d)) : rodaTudo xs

area :: Rectangulo -> Float
area ((a,b),(c,d)) = if (quadrado ((a,b),(c,d))) == False
                     then (a-c)^2*(b-d)^2
                     else 0 
areaTotal :: [Rectangulo] -> Float
areaTotal (((a,b),(c,d)):xs) = area ((a,b),(c,d)) + areaTotal xs 

escala :: Float -> Rectangulo -> Rectangulo
escala n ((a,b),(c,d)) = ((a,b) ,(c+n,d+n))

escalaTudo :: Float -> [Rectangulo] -> [Rectangulo]
escalaTudo n (((a,b),(c,d)):xs) = escala n ((a,b),(c,d)) : escalaTudo n xs

primos :: Int->Bool
primos 1 = False
primos 0 = False
primos (-1) = False
primos n = if n ==2 
           then True
           else if n==3
                then True
                else if mod n 2 == 0 
                     then False
                     else if mod n 3 == 0
                          then False
                          else True 

primeiro :: [(a,b)]->[a]
primeiro l = map (fst) l

nosPrimeiro :: (Ord a )=>a->[(a,b)]->Bool

nosPrimeiro a l = filter (==a) (primeiro l) == []

minFst :: (Ord a) => [(a,b)] -> a 
minFst l = mini (primeiro l)

mini :: (Ord a) => [a]->a
mini [x]= x
mini (x:xs) = min x (mini xs)

sndMinFst :: (Ord a) => [(a,b)] -> b

sndMinFst l =snd (head (ordena l))

ordena :: (Ord a) => [(a,b)]->[(a,b)]
ordena []=[]
ordena ((x,y):xs) = inser (x,y) (ordena xs) 

inser :: (Ord a) => (a,b) -> [(a,b)]-> [(a,b)]
inser (a,b) []= [(a,b)]
inser (a,b) ((x,y):xs) = if x>a
                         then (a,b):(x,y):xs
                         else (x,y):inser (a,b) xs 

ordenaSnd :: (Ord b) => [(a,b)] -> [(a,b)]
ordenaSnd []=[]
ordenaSnd ((x,y):xs) = inser2 (x,y) (ordenaSnd xs)

inser2 :: (Ord b) => (a,b) -> [(a,b)] -> [(a,b)]
inser2 (a,b) [] = [(a,b)]
inser2 (a,b) ((x,y):xs) = if y>b
                        then (a,b):(x,y): xs
                        else (x,y):inser2 (a,b) xs 
