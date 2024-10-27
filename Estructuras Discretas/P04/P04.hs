--Práctica 4 en Haskell - Funciones con nuevos tipos definidos
--Emiliano García Bollás. 27.10.24
--Líneas (Sin contar comentarios ni saltos de línea): 50
----------------- NUEVA ESTRUCTURA ------------------
data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving Show
-------------------- EJERCICIO 1 --------------------
longitud :: Arbol a -> Int 
longitud ArbolVacio = 0
longitud (Raiz a links rechts) = 1 + longitud links + longitud rechts

-------------------- EJERCICIO 2 --------------------
profundidad :: Arbol a -> Int 
profundidad ArbolVacio = 0
profundidad (Raiz a links rechts) = 1 + max (profundidad links) (profundidad rechts)

-------------------- EJERCICIO 3 --------------------
ancho :: Arbol a -> Int 
ancho ArbolVacio = 0
ancho (Raiz a ArbolVacio ArbolVacio) = 1
ancho (Raiz a links rechts) = ancho links + ancho rechts

-------------------- EJERCICIO 4 --------------------
data Recorrido  = InOrder | PreOrder | PosOrder

recorrido :: Arbol a -> Recorrido -> [a]
recorrido ArbolVacio r = []
recorrido (Raiz a ArbolVacio ArbolVacio) r = [a]
recorrido (Raiz a links rechts) InOrder  = recorrido links InOrder ++ [a] ++ recorrido rechts InOrder
recorrido (Raiz a links rechts) PreOrder  = [a] ++ recorrido links PreOrder ++ recorrido rechts PreOrder
recorrido (Raiz a links rechts) PosOrder = recorrido links PosOrder ++ recorrido rechts PosOrder ++ [a]

-------------------- EJERCICIO 5 --------------------
niveles :: Arbol a -> [[a]]
niveles ArbolVacio = []
niveles (Raiz a ArbolVacio ArbolVacio) = [[a]]
niveles (Raiz a links rechts) = [[a]] ++ prometheus (niveles links) (niveles rechts)

--Función auxiliar para unir elementos terminales
prometheus :: [[a]] -> [[a]] -> [[a]]
prometheus xs [] = xs
prometheus [] ys = ys
prometheus (x:xs) (y:ys) = (x ++ y) : prometheus xs ys

-------------------- EJERCICIO 6 --------------------
minimo :: Arbol a -> a 
minimo (Raiz a ArbolVacio rechts) = a
minimo (Raiz a links rechts) = minimo links

-------------------- EJERCICIO 7 --------------------
maximo :: Arbol a -> a 
maximo (Raiz a links ArbolVacio) = a
maximo (Raiz a links rechts) = maximo rechts

-------------------- EJERCICIO 8 --------------------
eliminar :: Ord a => Arbol a -> a -> Arbol a 

--Árbol vacío
eliminar ArbolVacio o = ArbolVacio

--Eliminación de hoja
eliminar (Raiz a ArbolVacio ArbolVacio) o =
  if a == o
  then ArbolVacio
  else (Raiz a ArbolVacio ArbolVacio)

--Eliminación de nodo con rama a la derecha
eliminar (Raiz a ArbolVacio rechts) o =
  if a == o
  then rechts
  else Raiz a ArbolVacio (eliminar rechts o)

--Eliminación de nodo con rama la izquierda
eliminar (Raiz a links ArbolVacio) o =
  if a == o
  then links
  else Raiz a (eliminar links o) ArbolVacio

--Eliminación de nodo con dos hijos
eliminar (Raiz a links rechts) o =
  if a == o
  then Raiz (maximo links) (eliminar links (maximo links)) rechts
  else Raiz a (eliminar links o) (eliminar rechts o)

