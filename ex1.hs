pir :: Float->Float

pir x=2*pi*x

dist :: (Float,Float)->(Float,Float)->Float

dist (x,y) (z,g) =sqrt((x-z)^2+(y-g)^2)

primUlt :: [Int]->(Int,Int)

primUlt l =(head l, head (reverse l))

multiplo :: Int->Int->Bool

multiplo m n= if div n m ==0
              then True
              else False

truncaImpar :: [Int]->[Int]

truncaImpar l = if ((mod (length l) 2)) == 0
                then l
                else tail l 

max2 :: Int->Int->Int

max2 x y= if x>y
          then x
          else y

max3 :: Int->Int->Int->Int
max3 x y z= if (max2 x y)>z
            then max2 x y
            else z

dist2 :: Float->Float->Float->Bool

dist2 x y z= if x+y >z && x+z>y && y+z>x
            then True
            else False

type Ponto = (Float,Float)
dist3 :: Ponto->Ponto->Ponto->(Float,Float,Float)

dist3 (x,y) (x1,y1) (x2,y2) = (dist (x,y) (x1,y2), dist (x,y) (x2,y2), dist (x2,y2) (x1,y1))

dist4 :: Ponto->Ponto->Ponto->Float

dist4 (x,y) (x1,y1) (x2,y2) = dist (x,y) (x1,y2) + dist (x,y) (x2,y2) + dist (x1,y1) (x2,y2)

dig :: Ponto->Ponto->[Ponto]

dig (x,y) (x1,y2) = (x,y):(x1,y2):(x1,y):(x,y2):[]
