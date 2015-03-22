data BTree a = Empty 
          | Node a (BTree a) (BTree a)
         deriving Show

altura ::(BTree a) -> Int
altura Empty=0
altura (Node a (e) (d)) = 1 + (max (altura e) (altura d))
contaNodos ::(BTree a) -> Int
contaNodos Empty = 0
contaNodos (Node a e d) = 1 + (contaNodos e) + (contaNodos d)
folhas :: (BTree a) -> Int
folhas Empty=0
folhas (Node a Empty Empty)=1
folhas (Node a d Empty ) = (folhas d)
folhas (Node a Empty e) =(folhas e)
folhas (Node a d e )=(folhas d) + (folhas e)
prune :: Int -> (BTree a) -> (BTree a)
prune 0 Empty= Empty
prune 0 (Node a _ _) = Empty
prune n (Node a e d) = if n>1 
                       then (Node a (prune (n-1) e) (prune (n-1) d))
                       else (Node a e d)

path :: [Bool] -> (BTree a) -> [a]
path [] Empty= []
path l Empty =[]
path [] (Node a e d) = []
path (True:xs) (Node a e d) = [a]++  path xs d
path (False:xs) (Node a e d) = [a] ++ path xs e 

mirror ::(BTree a) -> BTree a
mirror Empty=Empty
mirror (Node a e d) = (Node a (mirror d) (mirror e))
{--zipWithBT :: (a -> b -> c) -> (BTree a) -> (BTree b) -> BTree c
zipWithBT f Empty Empty =Empty
zipWithBT f (Node a _ _) Empty =Empty
zipWithBT f Empty (Node a _ _)=Empty
zipWithBT f (Node a e d) (Node a1 e1 d1) = (Node ((f a a1)) (zipWithBT (f e e1)) (zipWithBT (f d d1)))


unzipBT :: (BTree (a,b,c)) -> (BTree a,BTree b,BTree c)
unzipBT Empty =(Empty,Empty,Empty)
unzipBT ((Node a b c) e d) = let (e1,e2,e3)=unzipBT e
                                 (d1,d2,d3)=unzipBT d
                             in ((Node a e1 d1), (Node b e2 d2), (Node c e3 d3))
---}                                 

teste :: (Num a)=>BTree a
teste = Node 1 (Node 1 Empty Empty) (Node 1 Empty Empty)
