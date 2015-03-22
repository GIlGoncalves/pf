type Aluno = (Numero,Nome,ParteI,ParteII)
type Numero = Int
type Nome = String
type ParteI = Float
type ParteII = Float
type Turma = [Aluno]

tes :: Turma->Bool
tes []=False
tes [(n,no,p1,p2)]= if p1>=0 && p1<=12 && p2>=0 && p2<=8
                    then True
                    else False

tes l = if (tesTurma l) == True && (tesTurma1 l) == True
        then True
        else False

tesTurma :: Turma ->Bool
tesTurma [] = False
tesTurma [(x,y,p1,p2)] = if p1>=0 && p1<=12 && p2>=0 && p2<=8 
                        then True 
                        else False

tesTurma ((n,no,p1,p2):xs)= if verifica1 (n,no,p1,p2) 
                            then tesTurma xs
                            else False 


verifica1 :: Aluno->Bool
verifica1 (x,y,p1,p2) = if p1>=0 && p1<=12 && p2>=0 && p2<=8
                        then True 
                        else False
 




tesTurma1 :: Turma->Bool
tesTurma1 []=False
tesTurma1 [(n,no,p1,p2)]= True
tesTurma1 ((n,no,p1,p2):xs) = if (verifica (n,no,p1,p2) xs)
                             then False 
                             else tesTurma1 xs


verifica :: Aluno->Turma->Bool
verifica (x,m,n,b)[]= False
verifica (x,m,n,b) ((r,t,y,u):xs) = if x==r
                                    then True
                                    else verifica (x,m,n,b) xs

passa :: Turma->Int
passa [] = 0
passa ((x,y,p1,p2):xs) =  if tes ((x,y,p1,p2):xs) == False
                          then 0
                          else if p1 + p2 >=9.5
                               then 1 + passa xs
                               else passa xs

passaram :: Turma->[(Nome,Float)]
passaram [] = []
passaram [(x,j,p1,p2)]= if p1>=0 && p1<=12 && p2>=0 && p2<=8
                        then if p1+p2>=9.5
                             then [(j,p1+p2)]
                             else []
                             else []
                          

passaram ((x,j,p1,p2):xs) = if tes ((x,j,p1,p2):xs)==False
                           then []
                            else if (p1+p2) >= 9.5
                                then (j,p1+p2):passaram xs
                                else passaram xs

media :: Turma->Float
media [] = 0
media [(x,y,p1,p2)]= (p1+p2)/fromIntegral 2
media l = (soma (map (snd)(passaram l)))/ fromIntegral (length l)

soma :: [Float]->Float
soma [] = 0
soma (x:xs)= x + soma xs

nota :: Turma->Nome
nota []=""
nota [(x,y,p1,p2)]=y
nota l = fst(head ( reverse (ordena (passaram l))))

ordena :: [(Nome,Float)]->[(Nome,Float)]
ordena []=[]
ordena ((n,no):xs) = inser (n,no) (ordena xs)

inser :: (Nome,Float)->[(Nome,Float)]->[(Nome,Float)]
inser (x,y) []=[(x,y)]
inser (x,y) ((a,b):xs) = if b>y
                         then (x,y):(a,b):xs
                         else (a,b):inser (x,y) xs 
