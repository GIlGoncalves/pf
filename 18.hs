type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL 
              deriving (Eq,Show)
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
                   deriving (Eq,Show)
type Turma = BTree Aluno-- arvore binaria de procura (ordenada por numero)
data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving Show

inscNum :: Numero -> Turma -> Bool
inscNum n Empty= False
inscNum n (Node a Empty Empty)= n== retira a
inscNum n (Node a e Empty) = if n==retira a
                             then True
                             else if n<retira a
                                  then inscNum n e
                                  else False
inscNum n (Node a Empty d) = if n==retira a
                             then True
                             else if n>retira a
                                  then inscNum n d
                                   else False

inscNum n (Node a e d) = if n ==retira a
                        then True 
                        else if n>retira a
                           then inscNum n d
                         else inscNum n e



retira (a,b,c,d) = a
retira1 (a,b,c,d) =b
retira2 (a,b,c,d) =c
retira3 (a,b,c,d) =d
inscNome ::Nome -> Turma -> Bool
inscNome n Empty=False
inscNome n (Node a Empty Empty)= n==retira1 a
inscNome n (Node a e Empty) = if n==retira1 a
                              then True
                              else inscNome n e
inscNome n (Node a Empty d) = if n==retira1 a
                              then True
                              else inscNome n d
inscNome n (Node a e d) = if n==retira1 a
                          then True
                          else if (inscNome n e)==True ||(inscNome n d) ==True
                               then True
                               else False

trabEst ::Turma -> [(Numero,Nome)]
trabEst  Empty = []
trabEst (Node a Empty Empty) = if TE ==retira2 a
                               then [(retira a,retira1 a)]
                               else []
trabEst (Node a e Empty) = if TE == retira2 a
                           then (retira a,retira1 a):trabEst e
                           else trabEst e

trabEst (Node a Empty d) = if TE == retira2 a
                           then (retira a,retira1 a):trabEst d
                           else trabEst d
trabEst (Node a e d) = if TE == retira2 a
                       then [(retira a,retira1 a)]++(trabEst e) ++ (trabEst d)
                       else (trabEst e) ++ (trabEst d)

nota :: Numero -> Turma -> Maybe Classificacao
nota n Empty= Nothing
nota n (Node a e d )= let b =(inscNum1 n (Node a e d))
                      in if b==[]
                         then Nothing
                         else Just (snd (head b))






inscNum1 :: Numero -> Turma -> [(Bool,Classificacao)]
inscNum1 n Empty= []
inscNum1 n (Node a Empty Empty)= if n== retira a
                                 then [(True, retira3 a)]
                                 else []
inscNum1 n (Node a e Empty) = if n==retira a
                             then [(True,retira3 a)]
                             else if n<retira a
                                  then inscNum1 n e
                                  else []
inscNum1 n (Node a Empty d) = if n==retira a
                             then [(True,retira3 a)]
                             else if n>retira a
                                  then inscNum1 n d
                                   else []

inscNum1 n (Node a e d) = if n ==retira a
                        then [(True,retira3 a)]
                        else if n>retira a
                           then inscNum1 n d
                         else inscNum1 n e

percFaltas1 :: Turma -> Int
percFaltas1 Empty = 0
percFaltas1 (Node a Empty Empty) = if retira3 a == Faltou
                                 then 1
                                 else 0
percFaltas1 (Node a e Empty) = if retira3 a==Faltou
                              then 1 + percFaltas1 e
                              else percFaltas1 e
percFaltas1 (Node a Empty d) = if retira3 a==Faltou
                               then 1 + percFaltas1 d
                               else percFaltas1 d
percFaltas1 (Node a e d) = if retira3 a ==Faltou
                           then 1 + percFaltas1 e + percFaltas1 d
                           else percFaltas1 e + percFaltas1 d 


percFaltas :: Turma->Float
percFaltas Empty = 0.0
percFaltas (Node a e d) = fromIntegral (percFaltas1 (Node a e d))/ fromIntegral (altura(Node a e d))

altura :: Turma->Int
altura Empty= 0
altura (Node a Empty Empty)=1
altura (Node a e Empty) = 1 + altura e
altura (Node a Empty d) = 1 + altura d
altura (Node a e d) = 1+ max (altura e) (altura d) 

mediaAprov :: Turma -> Float 
mediaAprov Empty = 0
mediaAprov l =fromIntegral (percFaltas2 l)/fromIntegral (altura l)

percFaltas2 :: Turma -> Int
percFaltas2 Empty = 0
percFaltas2 (Node a Empty Empty) = if teste2(retira3 a)/=0
                                 then teste2(retira3 a)
                                 else 0
percFaltas2 (Node a e Empty) = if teste2(retira3 a)/=0
                              then teste2(retira3 a) + percFaltas2 e
                              else percFaltas2 e
percFaltas2 (Node a Empty d) = if teste2(retira3 a)/=0
                               then  teste2(retira3 a) + percFaltas2 d
                               else percFaltas2 d
percFaltas2 (Node a e d) = if teste2(retira3 a)/=0
                           then teste2(retira3 a)+ percFaltas2 e + percFaltas2 d
                           else percFaltas2 e + percFaltas2 d 

teste2 :: Classificacao->Int
teste2 (Aprov (a))=a
teste2 _=0
teste1 :: Classificacao->Bool
teste1 (Aprov (a)) = True
teste1 _ = False
{---
aprovAv :: Turma -> Float
aprovAv Empty = 0
aprovAv (Node a Empty Empty)=if teste1 (retira3 a) 
                             then 1 
                             else 0
aprovAv (Node a e Empty) = if teste1(retira3 a) 
                           then (1+ (aprovAv e))/(2+(aprovAv e))
                           else (aprovAv e)/(2+(aprovAv e))
 


aprovAv (Node a Empty d) = if teste1(retira3 a) 
                           then (1+ (aprovAv d))/(2+(aprovAv d))
                           else (aprovAv d)/(2+(aprovAv d))
aprovAv (Node a e d) = if teste1(retira3 a)
                       then  (1+(aprovAv e)+(aprovAv d))/(2+ max (aprovAv e) (aprovAv d))
                        else ((aprovAv e)+(aprovAv d))/(2+ max (aprovAv e) (aprovAv d))
--}
teste ::BTree Aluno
teste = Node (6,"ds",ORD,Aprov 3) (Node (5,"sd",ORD,Faltou) (Node (3,"dsa",TE, Aprov 4) Empty Empty) Empty) (Node (8,"dasad",TE, Faltou) Empty Empty)
