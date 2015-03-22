data Aposta = Ap [Int] (Int,Int)

valida :: Aposta -> Bool
valida (Ap l (x,y))= let l1 = filter (\x1-> x1>=1 && x1<=50) l
                     in if length l1 ==5 && x>=1 && x<=9 && y>=1 && y<=9 && length l ==5 && (repetidos l) == False
                       then True
                       else False 

repetidos:: [Int]->Bool
repetidos []=False
repetidos (x:xs)= if (repetidos1 x) xs 
                 then True
                 else repetidos xs 

repetidos1 :: Int->[Int]->Bool
repetidos1 a [] = False
repetidos1 a (x:xs)= if a==x
                     then True
                     else repetidos1 a xs


comuns :: Aposta -> Aposta -> (Int,Int)
comuns (Ap l (y,x1)) (Ap l1 (x,y1))=(comuns1 l l1,comuns1 [y,x1] [x,y1])



comuns1 :: [Int]->[Int]->Int
comuns1 [] [] = 0
comuns1 l []= 0
comuns1 [] l = 0
comuns1 (x:xs) l1=if length (filter (\x1-> x1==x) l1) ==1
                  then 1 + comuns1 xs l1
                  else comuns1 xs l1


instance (Eq Aposta) where 
         a==b = (comuns a b) ==(5,2)

premio :: Aposta->Aposta->Maybe Int
premio (Ap l x) (Ap l1 x1)=case (comuns (Ap l x) (Ap l1 x1)) 
                                     of 
                                        (5,2) -> Just 1
                                        (5,1) -> Just 2
                                        (5,0) -> Just 3
                                        (4,2) -> Just 4
                                        (4,1) -> Just 5
                                        (4,0) -> Just 6
                                        (3,2) -> Just 7
                                        (2,2) -> Just 8
                                        (3,1) -> Just 9
                                        (3,0) -> Just 10
                                        (1,2) -> Just 11
                                        (2,1) -> Just 12
                                        (2,0) -> Just 13 
                                        (1,0) -> Nothing
                                        (0,0) -> Nothing
                                        (0,1) -> Nothing
                                        (0,2) -> Nothing

leAposta :: IO Aposta
leAposta = do {
                putStrLn "Introduza uma lista de numeros  valida:    "
                x <- getLine
                putStrLn "Introduza as estrelas:    "
                y <- getLine}
                let j = (Ap (read x)  (read y))
                      in if (valida j)
                then return j
                else do {putStrLn "Aposta invalida"
                        ;leAposta} 
{--
joga :: Aposta -> IO ()
joga (Ap l x) = do 
                 j<-leAposta
               case (premio j (Ap l x)) 
               of 
           Just n -> putStrLn "Tem o  Premio " ++ (show n) ++ "º premio"
           Nothing -> putStrLn "Sem premio"
---}

geraChave::IO Aposta
geraChave = return (Ap [2,3,4,5,7] (3,8))
