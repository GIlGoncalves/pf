type Inscritos = [(Nume,Nome,Curso,Ano)]
type Nume = Integer
type Nome = String
type Curso = String
type Ano = Integer

aluca :: (Curso, Ano) -> Inscritos -> Int

aluca (c,g) [] = 0
aluca (c,g) l = length (filter(\(_,_,c1,a)-> c1==c && a==g) l) 

quantos :: Curso-> [Nume] -> Inscritos->Int
quantos a [] [] =0
quantos a _ [] =0
quantos a [] _ =0
quantos a c l = ty a(vis c l)

ty :: Curso->[Curso]->Int
ty a [] = 0
ty a (x:xs) = if a==x
              then 1 + ty a xs
              else ty a xs

vis :: [Nume]->Inscritos->[Curso]
vis [] [] =[]
vis _ [] =[]
vis [] _ =[]
vis (x:xs) l = (vi x l) :vis xs l

vi :: Nume->Inscritos->Curso
vi x [] =[]
vi x ((nu,n,c,a):xs1) = if x==nu
                        then (c) 
                        else vi x xs1


doAno :: Ano -> Inscritos -> [(Nume, Nome, Curso)]

doAno a [] =[]
doAno a l =map(\(nu,n,c,p)->(nu,n,c)) (filter (\(nu,n,c,p) -> a==p) l)
