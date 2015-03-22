data LTree a = Leaf a | Fork (LTree a) (LTree a)
            
instance (Eq a)=>(Eq (LTree a))  where
            a==b = (igualdade a) ==(igualdade b)


igualdade ::LTree a->[a]
igualdade (Leaf a)= [a]
igualdade (Fork a d)= igualdade a ++ igualdade d
{--
instance (Show a)=>Show (LTree a) where 
          show (Leaf a) = show a
          show (Fork e d) = "(" ++ show e ++ "/"++ "/"++ show d ++ ")" 
--}
mktree :: Int -> a -> LTree a
mktree 1 x= Leaf x
mktree n x = let n1 = div n 2
                 n2= n-n1
             in Fork (mktree n1 x) (mktree n2 x)

instance Show a=> Show (LTree a) where
            show (Leaf x) = show x
            show (Fork e d) = "."++(show e) ++"\n"++ "." ++ show d
