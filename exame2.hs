data Questionario = Solucao String
                  | Questao String Questionario Questionario

respostas ::Questionario -> [String]
respostas (Solucao a) = [a]
respostas (Questao a e d)= (respostas e) ++ (respostas d)

seqResp :: Questionario -> String -> Maybe [Bool]
seqResp (Solucao a) b = if a== b
                        then Just []
                        else Nothing
seqResp (Questao a e d) a1 
    | left  == Nothing && right == Nothing = Nothing
    | left  == Nothing = Just (True : r)
    | right == Nothing = Just (False  : l)
    where left     = seqResp e a1
          right    = seqResp d a1
          (Just l) = left
          (Just r) = right
                            
instance (Show Questionario) where 
         show (Solucao a)= "Solucao:  " ++ show a
         show (Questao a (Solucao b) d) = show a ++ "?" ++ "Nao  "++ "Solucao:" ++ show b
         show (Questao a e (Solucao b)) = show a ++ "?" ++ "Sim  " ++ "Solucao:" ++ show b
         show (Questao a e d) = show a ++ "?" ++ show e ++ show d 


   
 








q :: Questionario

q = Questao "a >= b" (Questao "a >= c" (Questao "b >= c" (Solucao "a b c") (Solucao "a c b")) (Solucao "c a b")) (Questao "a >= c" (Solucao "b a c") (Questao "b >= c"(Solucao "b c a")(Solucao "c b a")))

