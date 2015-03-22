type Polinomio = [Monomio]
type Monomio = (Float,Int)


conta :: Int -> Polinomio -> Int 
conta n [] = 0
conta n l =length (filter (\(x,h)->n==h) l)

selgrau :: Int -> Polinomio -> Polinomio

selgrau n [] =[]
selgrau n l = filter (\(x,g)->g>n) l

deriv :: Polinomio -> Polinomio

deriv p =filter (\(a,b)-> b>1) (map (\(x,f)->(x*fromIntegral f,f-1)) p)

calcula :: Float -> Polinomio -> Float

calcula n [] = 0.0
calcula n ((x,y):xs) = (x*cal n y) + calcula n xs
                        
cal :: Float->Int->Float
cal n 0 = 1
cal n 1 = n
cal n x = n*cal n (x-1) 

simp :: Polinomio -> Polinomio
simp [] =[]
simp l = filter (\(a,b)-> b/=0) l

mult :: Monomio -> Polinomio -> Polinomio
mult (c,e) p = map (\(a,b)-> (c*a,b+e)) p

type Mat a = [[a]]

dimOK :: Mat a -> Bool
dimOK [] = True
dimOK l = all1((map (length) l))

all1 :: [Int]->Bool
all1 [] = False
all1 (x:xs)= all(==x) xs

dimMat :: Mat a->(Int,Int)
dimMat [] = (0,0)
dimMat l = (length l, head(map (length) l))

addMat ::Num a=>Mat a ->Mat a->Mat a
addMat [] [] =[]
addMat (x:xs) (y:ys)= (zipWith (+) x y):addMat xs ys

transpose :: Mat a->Mat a
transpose [] =[]
transpose ([]:_) =[]
transpose l= (map (head) l):transpose (map (tail) l)

multMat ::Num a => Mat a ->Mat a->Mat a
multMat [] [] =[]
multMat [] _= []
multMat _[] =[]
multMat (l:g) k = map sum (map (zipWith(*) l) (transpose k)):multMat g k 

 
