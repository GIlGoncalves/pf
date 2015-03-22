toDigits :: Int -> [Int]

toDigits 0 = []
toDigits n = (mod n 10):toDigits (div n 10) 


fromDigits1 ::[Int]->Int
fromDigits1 [] =0 
fromDigits1 l = sum  (zipWith(*) [10^x | x<-[0..] ] l)

fromDigits ::[Int]->Int
fromDigits [] =0
fromDigits l= di 0 l 

di :: Int->[Int]->Int
di a [] =0
di a (x:xs)= x*10^a + di (a+1) xs   


fromDigits3 :: [Int]->Int
fromDigits3 [] =0
fromDigits3 l= (foldr (+) 0 (da 0 l))

da :: Int->[Int]->[Int]
da a [] =[]
da a (x:xs)= x*10^a:da (a+1) xs 
