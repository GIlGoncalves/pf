data Tree a = Empty
            | Leaf a
            | Fork (Tree a) (Tree a)


a1 :: Num a=>Tree a 
a1 = Fork (Fork (Leaf 4) (Fork Empty (Leaf 13 ))) (Fork Empty (Fork (Leaf 5) Empty))

a2 :: Num a=>Tree a
a2= Fork (Fork (Leaf 4) (Leaf 13)) (Leaf 5)

instance Show a=>Show (Tree a)  where 
         show Empty ="(" ++ "<"++ ">" ++ ")"
         show(Leaf a) =  "("++ show a ++")"
         show(Fork (e) (d))= "(" ++show (e) ++ "<-"++"*"++"->"++ show(d)++")"

ultimo :: Tree a -> Maybe a
ultimo Empty = Nothing
ultimo (Leaf x)= Just x
ultimo (Fork e Empty ) = (ultimo e)
ultimo (Fork Empty d) = (ultimo d)
ultimo (Fork e d) =(ultimo d)

apaga :: Eq a => a -> Tree a -> Tree a
apaga a Empty =Empty 
apaga a (Leaf x)= if x==a then Empty else Leaf x
apaga a (Fork e d)= (Fork (apaga a e) (apaga a d))

limpa :: (Tree a) -> (Tree a)
limpa Empty = Empty 
limpa (Leaf x) = (Leaf x)
limpa (Fork d Empty)= limpa d
limpa (Fork Empty e) =limpa e 
limpa (Fork e d) = (Fork (limpa e) (limpa d))
{--
randomRemove :: Tree a -> IO (Tree a)
randomRemove Empty = Leaf (randomRio (1,1000))
randomRemove (Leaf x) = (Leaf x)
---}
