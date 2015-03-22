type Hora = (Int,Int)
type Etapa = (Hora,Hora)
type Viagem = [Etapa]



testa :: Etapa->Bool

testa ((x,y),(a,b)) = if x>=1 && x<=24 && y>=0 && y<=59 && a>=1 && a<=24 && b>=0 && b<=59 && (x,y)<(a,b)
                      then True
                      else False
testa1 :: Etapa->Etapa->Bool
testa1 ((x,y),(z,h)) ((m,u),(i,o)) = if (z,h) < (m,u)
                                     then True 
                                     else False


testaViagem :: Viagem->Bool
testaViagem []=False
testaViagem [((m,u),(i,o))]= if testa ((m,u),(i,o))
                           then True
                           else False

testaViagem (x@((l,y),(z,h)):m@((c,u),(i,o)):xs)= testa x && testa1 ((l,y),(z,h)) ((c,u),(i,o)) && testaViagem(m:xs)

partida :: Viagem->(Int,Int)
partida [] = (0,0)
partida l= if testaViagem l
            then ((fst(fst(head l))),fst(snd(head(reverse l))))
            else (0,0)


calcula :: Viagem->Int
calcula [] =0
calcula [((x,y),(p,o))]=if testa ((x,y),(p,o))
                        then if x==y
                             then abs(y-o)
                             else (p*60-x*60) + abs(y-o)
                             else 0

calcula (((x,y),(p,o)):xs) = if testaViagem (((x,y),(p,o)):xs)
                             then if x==y
                                  then abs (y-o) + calcula xs
                                  else (p*60 -x*60) + abs(y-o) + calcula xs
                                  else 0

calcula1 :: Viagem->Int
calcula1 [] = 0
calcula1 [((x,p),(z,i))]=0
calcula1 (t@((x,p),(z,i)):m@((a,b),(c,d)):xs)= if testaViagem (t:xs)
                                               then if z==a
                                                    then abs(i-b)+calcula1 (m:xs)
                                                    else a*60-z*60 + abs (b-i) +calcula1 (m:xs)
                                                    else 0
calculaTudo :: Viagem->Int
calculaTudo []=0
calculaTudo l= if testaViagem l
               then calcula1 l + calcula l
               else 0 


type Ponto = (Float,Float) -- (abcissa,ordenada)
type Rectangulo = (Ponto,Float,Float) -- (canto sup.esq., larg, alt)
type Triangulo = (Ponto,Ponto,Ponto)
type Poligonal = [Ponto]
distancia :: Ponto -> Ponto -> Float
distancia (a,b) (c,d) = sqrt (((c-a)^2) + ((b-d)^2))

compr :: Poligonal->Float
compr [] = 0
compr [x]=0
compr (j@(x,y):k@(l,p):xs) = distancia (x,y) (l,p) + compr (k:xs)

triangulo :: Triangulo->Poligonal
triangulo (x,y,z) = x:y:z:[]

rectangulo :: Rectangulo->Poligonal
rectangulo ((x,y),g,o) = (x,y):(g,o):[]

fechada :: Poligonal->Bool
fechada [] = False
fechada [(x,y)]= True
fechada ((x,y):xs) = if distancia (x,y) (last xs) == 0
                     then True
                     else False

triangula :: Poligonal->[Triangulo]
triangula []=[]
triangula [x,y]=[]
triangula [x,y,z]= [(x,y,z)]
triangula (x:y:z:xs)= if fechada (x:y:z:xs)
                      then (x,y,z):triangula(x:z:xs)
                      else [] 



areaTriangulo (x,y,z) = let a = distancia x y
                            b = distancia y z
                            c = distancia z x
                            s = (a+b+c) / 2 -- semi-perimetro
                            in -- formula de Heron
                            sqrt (s*(s-a)*(s-b)*(s-c))
areaTr :: [Triangulo]->Float
areaTr [] =0.0

areaTr ((x,y,z):xs)= areaTriangulo (x,y,z) + areaTr xs

areaPoligonal :: Poligonal->Float
areaPoligonal [] = 0.0
areaPoligonal l = if fechada l
                 then areaTr (triangula l)
                 else 0.0

mover :: Ponto->Poligonal->Poligonal
mover (x,y) []= []
mover (x,y) ((z,h):xs) = if fechada ((z,h):xs)
                         then let a =filter (/= (z,h)) ((z,h):xs) 
                                  b= (y-h)
                               in (x,y):(sub b a) ++ [(x,y)]
                               else []
                                 
sub :: Float->Poligonal->Poligonal
sub n [] = []
sub n [(x,y)]= [(x,y+n)]
sub n ((x,y):xs) = (x,y+n):sub n xs


zoom2 :: Poligonal->Poligonal
zoom2 []=[]
zoom2 ((x,y):xs) = if fechada((x,y):xs)
                   then let a = filter (/= (x,y)) ((x,y):xs)
                         in (x,y):(dobro1(dobro a)) ++ [(x,y)]
                    else []

dobro :: Poligonal->Poligonal 
dobro [] =[]
dobro [(x,y)]=[(2*x-1,y)]
dobro ((x,y):xs) = (2*x-1,y):dobro xs

dobro1 :: Poligonal->Poligonal 
dobro1 []=[]
dobro1 [(x,y)]=[(x,2*y-1)]
dobro1 ((x,y):xs)=(x,2*y-1):dobro1 xs
