type Radar = [(Hora,Matricula,VelAutor,VelCond)]
type Hora = (Int,Int) ---------(horas,minutos)
type Matricula = String ---------matricula do carro em infraccao
type VelAutor =Int  --------velocidade autorizada
type VelCond = Float  ----velocidade do condutor

exc :: Matricula->Radar->[Float]
exc a [] =[]
exc a ((h,m,vela,velc):xs) = if a==m
                             then (velc-fromIntegral(vela)):exc a xs
                             else exc a xs

hora :: Hora->Radar->Int
hora (x,l) [] = 0
hora (x,l) ((h,m,vela,velc):xs)= if x==fst h
                                 then if (velc -fromIntegral(vela)) >0
                                      then 1 + hora (x,l) xs
                                      else hora (x,l) xs
                                else 0

ordem :: Radar->Radar
ordem [] =[]
ordem ((h,m,vela,velc):xs) = inser (h,m,vela,velc) (ordem xs)


inser :: (Hora,Matricula,VelAutor,VelCond)->Radar->Radar

inser (x,y,z,l) [] =[(x,y,z,l)]
inser (x,y,z,l) ((h,m,vela,velc):xs) = if fst h>fst x
                                       then (x,y,z,l):(h,m,vela,velc):xs
                                       else (h,m,vela,velc) : inser (x,y,z,l) xs

inf :: Radar->Bool

inf [] = False
inf ((h,m,vela,velc):xs) = inf1 (h,m,vela,velc) xs

inf1 :: (Hora,Matricula,VelAutor,VelCond)->Radar->Bool
inf1 (x,f,l,k) [] = False
inf1 (x,f,l,k) ((h,m,vela,velc):xs) = if fst x == fst h
                                      then True
                                      else inf1 (x,f,l,k) xs


maior :: Radar ->Float
maior [] =0
maior l = maximo(maior1 l)


maior1 :: Radar->[Float]
maior1 [] =[]
maior1 ((h,m,vela,velc):xs) = (velc-fromIntegral(vela)):maior1 xs

maximo :: [Float]->Float
maximo [x]=x
maximo (x:xs)= (max x (maximo xs))

menorPeriodo :: Radar-> Int
menorPeriodo [] =0
menorPeriodo [(x,y,z,h)]= snd x
menorPeriodo l = minimo1(diferenca(menor1(menor l)))

menor :: Radar->[Hora]
menor [] =[]
menor ((h,m,vela,velc):xs) = if (velc - fromIntegral (vela)) <0
                             then h:menor xs
                             else menor xs
menor1 :: [Hora]->[Int]
menor1 [] =[]
menor1 l= (map snd l)

diferenca :: [Int]->[Int]
diferenca [] =[]
diferenca [x]=[x]
diferenca (x:i@y:xs) = (y-x) :diferenca(y:xs)

minimo1 :: [Int]->Int
minimo1 [x]=x
minimo1 (x:xs)= (min x (minimo1 xs))    

apanhado :: Radar->Bool
apanhado [] = False
apanhado ((x,y,z,l):xs) = 

  
