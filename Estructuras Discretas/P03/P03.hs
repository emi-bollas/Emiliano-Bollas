-- PŔACTICA 3 EN HASKELL: FUNCIONES CON ESTRUCTURAS DEFINIDAS POR EL PROGRAMADOR --
        -- Emiliano García Bollás, Laboratorio de Estructuras Discretas --

-- Nueva estructura, provista por Raúl
data List a = Void | Node a (List a) deriving Show

--1 Función que calcula la cantidad de elementos de la nueva estructura List
longitud :: List a -> Int
longitud Void = 0
longitud (Node x xs) = 1 + longitud xs

--2 Función que revisa si un elemento está contenido en una lista
estaContenido :: Eq a => List a -> a -> Bool
estaContenido Void e = False
estaContenido (Node x xs) e =
  if x == e
  then True
  else estaContenido xs e

--3 Función que convierte una lista nativa de Haskell a una de la nueva estructura definida
convertirAEstructura :: [a] -> List a
convertirAEstructura [] = Void
convertirAEstructura (x:xs) = Node x (convertirAEstructura xs)

--4 Proceso visceverso al de la función anterior, transforma una lista de la nueva estructura a una nativa de Haskell
convertirALista :: List a -> [a]
convertirALista Void = []
convertirALista (Node x xs) = x : convertirALista xs

--5 Función que descarta elementos repetidos y conserva únicamente una aparición de cada uno
conjunto :: Eq a => List a -> List a
conjunto Void = Void
conjunto (Node x xs) =
  if estaContenido xs x 
  then conjunto xs
  else Node x (conjunto xs)

--6 (CORREGIDA: Eliminación de un if al incluir el caso que se buscaba como un caso base) Función que elimina un elemento en el índice indicado, considerando que el primer elemento está en la posición 0
eliminarIndice :: List a -> Int -> List a
eliminarIndice Void a = Void
eliminarIndice (Node x xs) 0 = xs
eliminarIndice (Node x xs) n =
  if n < 0 || n > longitud (Node x xs)-1
  then error "Índice fuera del rango permitido."
  else Node x (eliminarIndice xs (n-1))

--7 (CORREGIDA: Al igual que el anterior, se eliminó un if y se incluyó el caso buscado como un caso base, se ajustó la lógica de la función) Función que inserta un elemento en el índice indicado
insertarIndice :: List a -> Int -> a -> List a
insertarIndice Void 0 e = Node e (Void)
insertarIndice (Node x xs) 0 e = Node e (Node x xs)
insertarIndice (Node x xs) n e =
  if n < 0 || n > longitud (Node x xs) - 1
  then error "Índice fuera del rango permitido."
  else Node x (insertarIndice xs (n-1) e)

--8 ()Función que recorre a la derecha los elementos en la cabeza de una lista, que es lo mismo que recorrerla por completo hacia la izquieda.

recorrerLista :: List a -> Int -> List a
recorrerLista Void n = Void
recorrerLista (Node x (Node y ys)) n =
    if n > 0
    then recorrerLista (insertarIndice y 0 (Node x ys)) (n-1)
    else (Node x (Node y ys))

--Versión previa de la función recorrerLista que emplea una función auxiliar para trabajar con las propiedades de las funciones nativas de Haskell.
recorrerListaOLD :: List a -> Int -> List a
recorrerListaOLD Void a = Void
recorrerListaOLD (Node x xs) a =
  if a > 0
  then recorrerListaOLD (convertirAEstructura(adj(convertirALista(Node x xs)))) (a-1)
  else (Node x xs)
--X Función auxiliar que concatena la cabeza de una lista al final de la cola, ussada en el proceso de arriba
adj :: [a] -> [a]
adj [] = []
adj (x:xs) = xs ++ [x]
