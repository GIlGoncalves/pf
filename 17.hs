data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving Show

minimo :: (Ord a) => BTree a -> a
minimo Empty= error "Não funciona"
minimo (Node a Empty Empty) = a
minimo (Node a Empty d) = a
minimo (Node a e Empty) = minimo e 
minimo (Node a e d) = minimo e

semMinimo :: (Ord a) => BTree a -> BTree a
semMinimo Empty = error "Não funciona"
semMinimo (Node a Empty Empty)=Empty
semMinimo (Node a Empty d) =Empty
semMinimo (Node a e Empty) = (Node a (semMinimo e) Empty)
semMinimo (Node a e d) = (Node a (semMinimo e) d)

minSmin :: (Ord a) => BTree a -> (a,BTree a)
minSmin Empty = error "Não funciona"
minSmin (Node a Empty Empty) =(a,(Empty))
minSmin (Node a Empty d) =(a,(Empty))
minSmin (Node a e Empty) =(fst(minSmin e),(Node a (snd(minSmin e)) Empty))
minSmin (Node a e d) = (fst(minSmin e),(Node a (snd(minSmin e)) d))

maximo :: (Ord a) => BTree a -> a
maximo Empty = error "Não funciona"
maximo (Node a Empty Empty)= a
maximo (Node a e Empty) = a
maximo (Node a Empty d) = maximo d
maximo (Node a e d) = maximo d

semMaximo :: (Ord a) => BTree a -> BTree a
semMaximo Empty = error "Não funciona"
semMaximo (Node a Empty Empty) =Empty
semMaximo (Node a Empty d) =(Node a Empty (semMaximo d))
semMaximo (Node a e Empty) = Empty
semMaximo (Node a e d) = (Node a e (semMaximo d))

maxSmax :: (Ord a) => BTree a -> (a,BTree a)
maxSmax Empty = error "Não funciona"
maxSmax (Node a Empty Empty)= (a,Empty)
maxSmax (Node a e Empty) = (a, Empty)
maxSmax (Node a Empty d) = (fst(maxSmax d),(Node a Empty (snd(maxSmax d))))
maxSmax (Node a e d) = (fst(maxSmax d),(Node a e (snd(maxSmax d))))

teste :: (Num a)=>BTree a
teste = Node 6 (Node 5 (Node 3 Empty Empty) Empty) (Node 8 Empty Empty)
