data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show 

type Nome = String
type Agenda = [(Nome, [Contacto])]

acrescEmail :: Nome -> String -> Agenda -> Agenda

acrescEmail a ""[] =[]
acrescEmail a l [] = [(a,[Email l])]
acrescEmail a l ((n,l1):xs) = if a/=n
                              then (n,l1): acrescEmail a l xs
                              else if (compara l (retiraEmail l1))
                                   then (n,(Email l):l1):xs
                                   else (n,l1):xs    

retiraEmail :: [Contacto]->String
retiraEmail []=""
retiraEmail (Email x:[]) = x
retiraEmail (Email x:_)= x
retiraEmail (_:xs)=retiraEmail xs

compara :: String->String->Bool
compara "" "" = False
compara l "" = True
compara l l1 = if l1 ==l 
               then False 
               else True 


verEmails :: Nome -> Agenda -> Maybe [String]

verEmails a [] = Nothing
verEmails a ((x,l):xs) = if a ==x
                         then Just (retiraEmail1 l)
                         else verEmails a xs 

retiraEmail1 :: [Contacto]->[String]
retiraEmail1 []=[]
retiraEmail1 (Email x:[]) = [x]
retiraEmail1 (Email x:xs)= x:retiraEmail1 xs
retiraEmail1 (_:xs)=retiraEmail1 xs

consTelefs :: [Contacto] -> [Integer]

consTelefs [] = []
consTelefs ((Casa x):xs) =x:consTelefs xs
consTelefs ((Trab x):xs) =x:consTelefs xs
consTelefs ((Tlm x):xs) = x:consTelefs xs
consTelefs ((Email x):xs) = consTelefs xs

casa :: Nome -> Agenda -> Maybe Integer
casa a [] = Nothing 
casa a ((x,l):xs) = if a==x
                    then retiraTel l
                    else casa a xs 

retiraTel :: [Contacto]->Maybe Integer
retiraTel []=Nothing
retiraTel ((Casa x):_)=Just x
retiraTel ((Trab x):xs) = retiraTel xs
retiraTel ((Tlm x):xs) = retiraTel xs
retiraTel ((Email x):xs) = retiraTel xs  
                       
