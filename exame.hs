type TabClass= [(Piloto,Equipa,Pontos)]
type Piloto = String
type Pontos = Int
type Equipa = String

pontosEquipa :: Equipa -> TabClass -> Pontos 
pontosEquipa a [] = 0
pontosEquipa a ((x,y,p):xs) = if a==y
                              then p + pontosEquipa a xs
                              else pontosEquipa a xs

m1 :: [Pontos]
m1 = [25, 18, 15, 12, 10, 8, 6, 4, 2 ,1]

junta :: [Piloto] -> TabClass -> TabClass
junta [] [] =[]
junta _ [] =[]
junta [] l =[]
junta l f = ac l m1 f  



ac::[Piloto]->[Int]->TabClass->TabClass
ac [] [] [] =[]
ac l [] [] =[]
ac [] l [] =[]
ac [] [] l =[]
ac (x:xs) (x1:xs1) l = adiciona (verifica x l) x1 : ac xs xs1 l

adiciona :: (Piloto,Equipa,Pontos)-> Int->(Piloto,Equipa,Pontos)
adiciona (p,e,po) n = (p,e,po+n)

verfica :: Piloto->TabClass->(Piloto,Equipa,Pontos) 
verfica a [] =("","",0)
verifica a ((p,e,po):xs) = if a ==p
                           then (p,e,po)
                           else verifica a xs       
                       


type Mat a = [[a]]

rotateLeft :: Mat a-> Mat a
rotateLeft ([]:_) =[]
rotateLeft l =(map last l):rotateLeft (map (init) l)

triSup :: (Num a,Eq a)=>Mat a -> Bool
triSup (_:[]) = True

triSup (x:y:xs) = if ((veri1 x) <(veri1 y))
                  then triSup (y:xs)
                  else False

veri1 :: (Eq a,Num a)=>[a]->Int
veri1 [] =0
veri1 l = length (takeWhile (==0) l)


tras :: Mat a->Mat a
tras ([]:_) =[]
tras l = (map head l):tras( map (tail) l) 

