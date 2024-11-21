--- Emiliano García Bollás
data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord)

data Formula = Atom Var
               |Neg Formula
               |Formula :&: Formula
               |Formula :|: Formula
               |Formula :=>: Formula
               |Formula :<=>: Formula deriving (Show, Eq, Ord)
infixl 9 :&:
infixl 9 :|:
infixl 7 :=>:
infixl 8 :<=>:
  
-------------------- EJERCICIO 1 --------------------
variables :: Formula -> [Var]
variables (Atom a) = [a]
variables (Neg f) = conjunto(variables f)
variables (f1 :&: f2) = conjunto(variables f1 ++ variables f2)
variables (f1 :|: f2) = conjunto(variables f1 ++ variables f2)
variables (f1 :=>: f2) = conjunto(variables f1 ++ variables f2)
variables (f1 :<=>: f2) = conjunto(variables f1 ++ variables f2)
----------Adaptación de conjunto de P03.hs-----------
conjunto :: [Var] -> [Var]
conjunto [] = []
conjunto (x:xs) = if estaContenido xs x
                  then conjunto xs
                       else x : conjunto xs
-------Auxiliar de conjunto, también de P03.hs-------                            
estaContenido :: [Var] -> Var -> Bool
estaContenido [] q = False
estaContenido (x:xs) q = if x == q
                            then True
                                 else estaContenido xs q
-----------------------------------------------------

-------------------- EJERCICIO 2 --------------------
negacion :: Formula -> Formula
negacion (Atom a) = Neg (Atom a)
negacion (f1 :&: f2) = negacion f1 :|: negacion f2
negacion (f1 :|: f2) = negacion f1 :&: negacion f2
negacion (f1 :=>: f2) = f1 :&: negacion f2
negacion (f1 :<=>: f2) = (f1 :&: negacion f2) :|: (negacion f1 :&: f2)
-----------------------------------------------------

-------------------- EJERCICIO 3 --------------------
equivalencia :: Formula -> Formula
equivalencia (Atom a) = Atom a --Posiblemente de más
equivalencia (f1 :=>: f2) = negacion f1 :|: f2
equivalencia (f1 :<=>: f2) = (f1 :&: f2) :|: (negacion f1 :&: negacion f2)
-----------------------------------------------------

-------------------- EJERCICIO 4 --------------------
interpretacion :: Formula -> [(Var,Bool)] -> Bool
interpretacion (Atom a :=>: Atom b) [(a,m),(b,n)] = not m || n -- Equivalencia lógica
interpretacion (Atom a :&: Atom b) [(a,m),(b,n)] = m && n 
interpretacion (Atom a :|: Atom b) [(a,m),(b,n)] = m || n
interpretacion (Atom a :<=>: Atom b) [(a,m),(b,n)] = interpretacion (Atom a :=> Atom b) [(a,m),(b,n)] && 
                                                      interpretacion (Atom b :=>: Atom a) [(a,m),(b,n)]
interpretacion (f1 :&: f2) comp = interpretacion f1 comp && interpretacion f2 comp
interpretacion (f1 :|: f2) comp = interpretacion f1 comp || interpretacion f2 comp
interpretacion (f1 :=>: f2) comp = not (interpretacion f1 comp) || interpretacion f2 comp
interpretacion (f1 :<=>: f2) comp = interpretacion (f1 :=>: f2) comp && 
                                                                  interpretacion (f2 :=>: f1) comp
                                    

-----------------------------------------------------

-------------------- EJERCICIO 5 --------------------
combinaciones :: Formula -> [[(Var,Bool)]]
combinaciones _ = undefined
-----------------------------------------------------

-------------------- EJERCICIO 6 --------------------
tablaDeVerdad :: Formula -> [([(Var,Bool)],Bool)]
tablaDeVerdad _ = undefined
-----------------------------------------------------
