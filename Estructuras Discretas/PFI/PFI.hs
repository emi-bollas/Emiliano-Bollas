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
negacion (Neg fn) = fn
negacion (f1 :&: f2) = (negacion f1) :|: (negacion f2)
negacion (f1 :|: f2) = (negacion f1) :&: (negacion f2)
negacion (f1 :=>: f2) = (f1) :&: (negacion f2)
negacion (f1 :<=>: f2) = (f1 :&: negacion f2) :|: (negacion f1 :&: f2)
-----------------------------------------------------
-------------------- EJERCICIO 3 --------------------
equivalencia :: Formula -> Formula
equivalencia (Atom a) = Atom a
equivalencia (Neg fn) = negacion (equivalencia fn)
equivalencia (f1 :&: f2) = (equivalencia f1) :&: (equivalencia f2)
equivalencia (f1 :|: f2) = (equivalencia f1) :|: (equivalencia f2)
equivalencia (f1 :=>: f2) = negacion (equivalencia f1) :|: (equivalencia f2)
equivalencia (f1 :<=>: f2) = (equivalencia f1 :&: equivalencia f2) :|: (negacion (equivalencia f1) :&: negacion (equivalencia f2))
-----------------------------------------------------
-------------------- EJERCICIO 4 --------------------
--Función que dado una variable y una lista de tuplas (Var, Bool) devuelve el valor booleano asociado a la variable
suchen :: Var -> [(Var, Bool)] -> Bool
suchen a [] = error "No todas las variables están definidas"
suchen a ((x, boo):xs) = if a == x
                         then boo 
                         else suchen a xs
interpretacion :: Formula -> [(Var, Bool)] -> Bool
interpretacion (Atom a) con = suchen a con
interpretacion (Neg fn) con = not (interpretacion fn con)
interpretacion (f1 :&: f2) vals = (interpretacion f1 vals) && (interpretacion f2 vals)
interpretacion (f1 :|: f2) vals = (interpretacion f1 vals) || (interpretacion f2 vals)
interpretacion (f1 :=>: f2) vals = not (interpretacion f1 vals) || (interpretacion f2 vals)
interpretacion (f1 :<=>: f2) vals = (interpretacion f1 vals) == (interpretacion f2 vals)
-----------------------------------------------------
-------------------- EJERCICIO 5 --------------------
combinaciones :: Formula -> [[(Var,Bool)]]
combinaciones fn = prol (asociacion (variables fn))
--Función auxiliar que recibe una lista de variables y devuelve una tupla de la misma con un valor booleano
asociacion :: [Var] -> [[(Var, Bool)]]
asociacion [] = [[]]
asociacion (x:xs) = [(x, False):ys | ys <- asociacion xs] ++ [(x, True):ys | ys <- asociacion xs]
--Función auxiliar que continúa agrupando tuplas de variables y booleanos
prol :: [[(Var, Bool)]] -> [[(Var, Bool)]]
prol [] = []
prol (x:xs) = x : prol xs
-----------------------------------------------------
-------------------- EJERCICIO 6 --------------------
tablaDeVerdad :: Formula -> [([(Var,Bool)],Bool)]
tablaDeVerdad fn = [(x, interpretacion fn x) | x <- combinaciones fn]
-----------------------------------------------------
--25-11-24 18:40 - Últimos cambios--
