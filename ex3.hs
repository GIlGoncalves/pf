dobros :: [Float]->[Float]

dobros [] = []

dobros (x:xs)=2*x:dobros xs

ocorre :: Char -> String -> Int

ocorre a []= 0

ocorre a (x:xs)= if a==x
                 then 1 + ocorre a xs
                 else ocorre a xs

pmaior :: Int -> [Int] -> Int

pmaior n [] = n

pmaior n (x:xs) = if x>n
                  then x
                  else pmaior n xs

repetidos ::[Int] -> Bool 

repetidos [] = False
repetidos (x:xs) = if testa x xs 
                   then True
                   else repetidos xs 

testa :: Int->[Int]->Bool

testa x []=False
testa x (y:ys)= if x==y
                then True
                else testa x ys

tresUlt :: [a] -> [a]

tresUlt [] = []
tresUlt l= if length l < 3
           then l
           else tak 3 (reverse l)

tak :: Int->[a]->[a]

tak 0 l = []
tak n [] = []
tak n (x:xs)= x : tak (n-1) xs

posImpares :: [a] -> [a]

posImpares [] = []
posImpares [x]= [x]
posImpares (x:xs)= x:posImpares(tail xs)


retira ::Int->[a]->a

retira 0 (x:xs)= x
retira n (x:xs)= retira (n-1) xs  

somaNeg :: [Int] -> Int 
somaNeg []=0
somaNeg (x:xs)= if x<0
                then x + somaNeg xs
                else somaNeg xs
ordena :: [Int]->[Int]
ordena []=[]
ordena (x:xs) = inser x (ordena xs)

inser :: Int->[Int]->[Int]
inser x []= [x]
inser x (y:ys) = if y>x
                 then x:y:ys
                 else y:inser x ys


type Jogo = (String,Int,String,Int) -- (Eq.casa,golos,Eq.visitante,golos)

golosEquipa ::Jogo->String->Int

golosEquipa (c,gc,v,gv) a = if c==a 
                            then gc
                            else if v==a
                                 then gv
                                 else -1 

resJogo :: Jogo->Char

resJogo (c,gc,v,gv) = if gc==gv
                      then 'x'
                      else if gc>gv
                           then '1'
                           else '2' 

resJogo1 :: Jogo->String
resJogo1 (c,gc,v,gv)= if gc>gv
                     then "Ganhou a equipa da casa"
                     else if gc==gv
                          then "Empataram"
                          else "Ganhou a equipa visitante"

jogos :: [Jogo]->Int

jogos [] = 0
jogos ((c,gc,v,gv):xs)= if gc==gv
                        then 1+ jogos xs
                        else jogos xs

jogosComxGolos:: [Jogo] -> Int -> Int
jogosComxGolos [] n = 0
jogosComxGolos ((c,gc,v,gv):xs) n = if n==(gc+gv)
                                    then 1 + jogosComxGolos xs n
                                    else  jogosComxGolos xs n

visi :: [Jogo]->Int
visi [] = 0
visi ((c,gc,v,gv):xs) = if resJogo (c,gc,v,gv) == '2'
                        then 1 + visi xs
                        else visi xs


