--type Dakar = [Piloto]
data Piloto = Carro Numero Nome Categoria |Mota Numero Nome Categoria |Camiao Numero Nome
             deriving Show 
data Categoria = Compticao |Maratona
             deriving Show
type Numero = Int
type Nome= String
{--
inserePil ::Piloto -> Dakar -> Dakar
inserePil a [] = [a]
inserePil ((Carro a n m)) ((Carro x y c):xs)= if n==y
                                              then (Carro a n m):(Carro x y c):xs
                                              else if  n>y
                                                   then (Carro x y c):inserePil (Carro a n m) xs
                                                   else (Carro a n m):(Carro x y c):xs
inserePil ((Carro a n m)) ((Mota x y c):xs)= if n==y
                                              then (Carro a n m):(Mota x y c):xs
                                              else if  n>y
                                                   then (Mota x y c):inserePil (Carro a n m) xs
                                                   else (Carro a n m):(Mota x y c):xs
inserePil ((Carro a n m)) ((Camiao x y):xs)= if n==y
                                              then (Carro a n m):(Camiao x y):xs
                                              else if  n>y
                                                   then (Camiao x y):inserePil (Carro a n m) xs
                                                   else (Carro a n m):(Camiao x y):xs
inserePil ((Mota a n m)) ((Carro x y c):xs)= if n==y
                                              then (Mota a n m):(Carro x y c):xs
                                              else if  n>y
                                                   then (Carro x y c):inserePil (Mota a n m) xs
                                                   else (Mota a n m):(Carro x y c):xs
inserePil ((Mota a n m)) ((Mota x y c):xs)= if n==y
                                              then (Mota a n m):(Mota x y c):xs
                                              else if  n>y
                                                   then (Mota x y c):inserePil (Mota a n m) xs
                                                   else (Mota a n m):(Carro x y c):xs
inserePil ((Mota a n m)) ((Camiao x y):xs)= if n==y
                                              then (Mota a n m):(Camiao x y):xs
                                              else if  n>y
                                                   then (Camiao x y):inserePil (Mota a n m) xs
                                                   else (Mota a n m):(Camiao x y):xs
inserePil ((Camiao a n)) ((Carro x y c):xs)= if n==y
                                              then (Camiao a n):(Carro x y c):xs
                                              else if  n>y
                                                   then (Carro x y c):inserePil (Camiao a n) xs
                                                   else (Camiao a n):(Carro x y c):xs
inserePil ((Camiao a n)) ((Mota x y c):xs)= if n==y
                                              then (Camiao a n):(Mota x y c):xs
                                              else if  n>y
                                                   then (Mota x y c):inserePil (Camiao a n) xs
                                                   else (Camiao a n):(Mota x y c):xs
inserePil ((Camiao a n)) ((Camiao x y):xs)= if n==y
                                              then (Camiao a n):(Camiao x y):xs
                                              else if  n>y
                                                   then (Camiao x y):inserePil (Camiao a n) xs
                                                   else (Camiao a n):(Camiao x y):xs

ordenaPiloto :: Dakar->Dakar
ordenaPiloto [] =[]
ordenaPiloto (x:xs)= (inserePil x) (ordenaPiloto xs)
--}
data BTree a = Vazia | Nodo a (BTree a) (BTree a)
              deriving Show
type Dakar = BTree Piloto
maior :: Dakar -> Piloto
maior Vazia= error "não funciona"
maior (Nodo a Vazia Vazia)= a
maior (Nodo a e Vazia)=a
maior (Nodo a Vazia d)= maior d
maior (Nodo a e d)= maior d 

menor :: Dakar -> Piloto
menor Vazia = error "não funciona"
menor (Nodo a Vazia Vazia) =a
menor (Nodo a Vazia d) = a
menor (Nodo a e Vazia) = menor e
menor (Nodo a e d)= menor e

listaMotas :: Dakar -> [(Numero, Nome)]
listaMotas Vazia = error "não funciona"
listaMotas (Nodo a Vazia Vazia)= verifica a
listaMotas (Nodo a e Vazia) = let j=(verifica a) ++ (listaMotas e)
                               in ordena j
                                 
listaMotas (Nodo a Vazia d) = let j=(verifica a) ++ (listaMotas d)
                              in ordena j
listaMotas (Nodo a e d) = let j =(verifica a) ++ (listaMotas e) ++ (listaMotas d)
                           in ordena j  
verifica :: Piloto->[(Numero,Nome)]
verifica (Mota a b c) = [(a,b)]
verifica (Carro a b c)= []
verifica (Camiao a b) = [] 

ordena :: [(Numero,Nome)]->[(Numero,Nome)]
ordena []= []
ordena ((x,y):xs)= inser (x,y) (ordena xs)

inser :: (Numero,Nome)->[(Numero,Nome)]->[(Numero,Nome)]
inser (a,b) [] =[(a,b)]
inser (a,b) ((x,y):xs)= if x>a
                        then (a,b):(x,y):xs
                        else (x,y):inser (a,b) xs
