type Album = (Titulo,Artista,Ano,[Musica])
type Musica = (Nome,Int) -- (nome da musica, durac~ao em segundos)
type Titulo = String
type Nome = String
type Artista = String
type Ano = Int

doArtista :: [Album] -> Artista -> [(Titulo,Ano)]

doArtista [] a=[]
doArtista [(x,y,z,l)] a = if y==a
                          then [(x,z)]
                          else []
doArtista ((x,y,z,l):xs) a = if y==a
                             then (x,z):doArtista xs a
                             else doArtista xs a



conta :: [Artista] -> [Album] -> [(Artista,Int)]

conta [] [] =[]
conta l [] = []
conta [] m = []
conta (x:xs) l = (x,conta1 x l):conta xs l 
                 
 
conta1 :: Artista->[Album]->Int
conta1 a [] = 0
conta1 a [(x,y,z,l)]= if a==y
                      then 1
                       else 0
conta1 a ((x,y,z,l):xs)= if a==y
                         then 1 + conta1 a xs
                         else conta1 a xs

fun :: Artista -> [Album] -> [(Titulo,Int)]
fun a [] = []
fun a ((t,a1,an,l):xs) = if a==a1
                         then (t,suma l):fun a xs
                         else fun a xs

suma :: [Musica]->Int
suma []=0
suma ((x,y):xs)=y+suma xs

{---fun :: Artista -> [Album] -> [(Titulo,Int)]
fun x l = map aux (filter (\(_,a,_,_)->x==a) l)
          where aux (t,_,_,m) = (t, sum (map snd m))
--}

maisAntigos :: [Album] -> [Album]
maisAntigos [] =[]
maisAntigo ((t,ar,an,l):xs) = inser (t,ar,an,l) (maisAntigo xs)

inser :: Album->[Album]->[Album]
inser (t,ar,an,l) [] = [(t,ar,an,l)]
inser (t,ar,an,l) ((r,j,k,n):xs) = if k>an
                                   then (t,ar,an,l):(r,j,k,n):xs
                                   else (r,j,k,n):inser (t,ar,an,l) xs 

								   


m1 :: [Musica]
m1 =[("gil",5),("jorge",6),("and",7)]	
m2 :: [Musica]
m2 =[("jorge",6),("and",7),("car",8)]	
m5 :: [Musica]
m5=[("gil",6),("poeta",7)]
m3 :: [Album]
m3 = [("gil","batatas",2013,m1),("sd","ssdf",333,m7)]
m6 :: Album
m6 = ("fd","dsd",555,m5)
m4 ::[Album]
m4 = [("gs","ads",2014,m1),("dsa","ty",2312,m1),("gi","sdsd",2013,m5)]
m7 :: [Musica]
m7 =[("sds",7)]

covers :: [Album] -> [(Nome,[(Artista,Ano)])]
covers [] =[]
covers l = remover(verifica1(gil l))

remover ::[(Nome,[(Artista,Ano)])]->[(Nome,[(Artista,Ano)])]
remover [] =[]
remover ((n,l):xs)= (n,l):remover(filter(\(m,_)->m/=n) xs)

gil :: [Album]->[(Artista,Ano,Nome)]
gil [] =[]
gil ((t,ar,an,l):xs) = (daOsNomes (ar,an,l) (ar,an,l)) ++ gil xs  


daOsNomes :: (Artista,Ano,[Musica])->(Artista,Ano,[Musica])->[(Artista,Ano,Nome)]
daOsNomes (ar,an,[]) (ar1,an1,[])=[]
daOsNomes (ar,an,(x,y):xs) (ar1,an1,(x1,y1):xs1) = if ar==ar1
                                                   then [(ar,an,x)] ++ daOsNomes (ar,an,xs) (ar1,an1,xs1)
												   else []
											
verifica1 :: [(Artista,Ano,Nome)]->[(Nome,[(Artista,Ano)])]
verifica1 [] =[]
verifica1 [(ar,an,n)]= [(n,[(ar,an)])]
verifica1 ((ar,an,n):xs) = (verifica (ar,an,n) xs) ++ verifica1 xs

verifica :: (Artista,Ano,Nome)->[(Artista,Ano,Nome)]->[(Nome,[(Artista,Ano)])]
verifica (ar,an,n) [] =[(n,[(ar,an)])]
verifica (ar,an,n) ((ar1,an1,n1):xs) = if n==n1
                                       then (n, [(ar,an)]++ri (ar,an,n) ((ar1,an1,n1):xs)):verifica (ar,an,n) xs
									   else verifica (ar,an,n) xs
									  
ri :: (Artista,Ano,Nome)->[(Artista,Ano,Nome)]->[(Artista,Ano)]
ri (ar,an,n) [] =[]
ri (ar,an,n) ((ar1,an1,n1):xs) = if n==n1
                                 then ((ar1,an1):ri (ar,an,n) xs) 
                                 else ri(ar,an,n) xs								 