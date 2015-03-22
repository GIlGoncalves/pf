type Mat a = [[a]]

dimOK :: Mat a -> Bool

dimOK [] = False
dimOK (y:l) = iguais(map length l)


iguais ::[Int]->Bool
iguais [] =True
iguais (x:xs) = all(==x) xs

dimMat :: Mat a -> (Int,Int)
dimMat []= (0,0)
dimMat ([]:_) =(0,0)
dimMat l = ((length l), length(map (length) l))

addMat ::(Num a)=> Mat a -> Mat a -> Mat a 
addMat [] []=[]
addMat (x:xs) (y:ys) = (zipWith (+) x y):(addMat xs ys)


m1 ::[[Int]]
m1 = [[3,6,9],[4,7,5],[4,7,9],[4,8,9]]

transpose :: Mat a -> Mat a
transpose ([]:_)=[]
transpose m = (map head m):transpose (map tail m)

multMat :: (Num a) => Mat a -> Mat a -> Mat a
multMat [] _ = []
multMat (x:xs) ys = map sum (map (zipWith (*) x) (transpose ys)) : multMat xs ys


ver :: Mat a->(Int,Int)->a
ver (x:xs) (a,b) = if a==1
                   then x !!(b-1)
                   else ver xs (a-1,b)





type Jornada = [Jogo]
type Jogo = ((Equipa,Golos),(Equipa,Golos)) -- (eq. casa, eq. visitante)
type Equipa = String
type Golos = Int 

totalGolos :: Jornada -> Int 

totalGolos [] =0
totalGolos (((e,g),(e1,g1)):xs)= g+g1+totalGolos xs

numGolos :: Int -> Jornada -> [Jogo]
numGOlos n [] =[]
numGolos n l = filter(\((e,g),(e1,g1))-> n<g || n<g1) l

venceCasa :: Jornada -> [Equipa]
venceCasa j = map (\((e,g),(_,_))->e) (filter (\((e,g),(e1,g1))-> g>g1) j)

pontos :: Jornada -> [(Equipa,Int)]

pontos [] =[]
pontos (((e,g),(e1,g1)):xs) = if g>g1
                              then (e,3):(e1,0):pontos xs
                              else if g==g1
                                   then (e,1):(e1,1):pontos xs
                                   else (e,0):(e1,3):pontos xs

empates :: Jornada -> [Jogo]
empates [] =[]
empates l = filter (\((e,g),(e1,g1))->g==g1 || g==g1) l

golosMarcados :: Jornada -> Int
golosMarcados j = sum (map (\((_,g),(_,g1))->g+g1) j)
