data Frac = F Integer Integer

           
normaliza :: Frac -> Frac
normaliza (F _ 0) = error "não funciona"
normaliza (F 0 _) = (F 0 0)
normaliza (F n m) = if n<0 && m<0
                  then let j =(mdc (abs n) (abs m))
                       in (F (div ((abs n)) j) (div (abs m) j))
                  else if n<0 ||  m<0 
                       then let j = (mdc (abs n) (abs m))
                             in (F (div (-(abs n)) j) (div (abs m) j))
                       else let j = (mdc (abs n) (abs m))
                             in (F (div (abs n) j) (div (abs m) j))


mdc :: Integer->Integer->Integer
mdc n m = if n==m
          then n
          else if n>m
               then mdc(n-m) m
               else  mdc n (m-n)

maximo :: [Int]->Int
maximo  = foldr (\n-> max n ) 0


data ExpInt =Const Int
            |Simetrico ExpInt
            |Mais ExpInt ExpInt 
            |Menos ExpInt ExpInt
            |Mult ExpInt  ExpInt 

calcula :: ExpInt -> Int
calcula (Const (x)) = x
calcula (Simetrico (x)) = -(calcula x)
calcula (Mais (x) (y)) = (calcula x) + (calcula y)
calcula (Menos (x) (y)) = (calcula x) - (calcula y)
calcula (Mult (x) (y)) = (calcula x) * (calcula y)

instance Show ExpInt where          
         show (Const x) = show x
         show(Simetrico x) = "("++ "-" ++ show (x) ++ ")"
         show(Mais x y)= "("++ show (x)  ++ "+" ++ show (y) ++ ")"
         show(Menos x y) = "(" ++ show (x) ++ "-" ++ show (y) ++ ")"
         show(Mult x y) = "(" ++ show (x) ++ "*" ++ show (y) ++ ")"

posfix :: ExpInt -> String
posfix (Const x) = show x
posfix (Simetrico x) = "-"++show x
posfix (Mais x y) = posfix x ++" " ++ posfix y ++" " ++ "+"
posfix (Menos x y) = posfix x ++ " " ++ posfix y ++ " " ++ "-"
posfix (Mult x y) = posfix x ++ " " ++ posfix y ++" " ++"*"

instance Eq ExpInt where 
         a==b = (calcula a) ==(calcula b)

instance Num ExpInt where
         (+) x y = Mais x y
         (-) x y = Menos x y
         (abs) x = Mult (signum x) x
         negate x = Simetrico x
         signum x = Const (signum (calcula x))
         fromInteger x = Const (fromInteger x)
         (*) x y = Mult x y 


data ExpN = N [Parcela]
            
type Parcela = [Int]
calcN ::ExpN -> Int

calcN (N [])=0
calcN (N l)= sum (map (sum) l)

normailiza2 ::ExpInt->ExpN
normailiza2 x=N [(normaliza1 x)]


normaliza1 ::ExpInt -> Parcela
normaliza1 (Const x)= [x]
normaliza1 (Simetrico x) =map(negate) (normaliza1 x)
normaliza1 (Mais x y)=  normaliza1 x ++ normaliza1 y
normaliza1 (Menos x y)= normaliza1 x ++ normaliza1 (Simetrico y)
normaliza1 (Mult x y) = normaliza1 x ++ normaliza1 (y)
