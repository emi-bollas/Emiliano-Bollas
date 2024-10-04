-----------LISTAS Y RECURSIÓN-------------

-- Longitud de una lista
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- Suma de n números
sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

-- CORREGIDO (Eliminación de un caso base innecesario y modificación para la acepción de la lista xs (cualquiera))- Agregar elemento a una lista
agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento xs a bool =
  if bool
  then a:xs
  else xs++[a]

-- Máximo de una lista
maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [] = error "¡Ahí no hay nada!"
maximoLista [a] = a
maximoLista (x:xs) =
  if x > maximoLista xs
  then x
  else maximoLista xs

-- CORREGIDO (Únicamente lista vacía y con el patrón x:xs, reducción de ifs de 3 a 2)- Recuperar un elemento de una lista de acuerdo a su índice (con comienzo en 0)
indice :: [a] -> Int -> a
indice [] int = error "¡No hay nada que extraer!"
indice (x:xs) int =
  if int < 0 || int > longitud (x:xs)-1
  then error "No hay ningún elemento bajo tal índice"
  else if int == 0
       then x
       else indice xs
            (int-1)                   

-----------LISTAS POR COMPRENSIÓN Y RECURSIÓN POR COLA-----------

-- CORREGIDO (Se quitó el caso base) - Divisores de un número entero
divisores :: Int -> [Int]
divisores int = [i | i<-[1..int],int `mod `i ==0]
  
-- Convertir una lista en conjunto
conjunto :: Eq a => [a] -> [a]
conjunto [a] = [a]
conjunto [] = error "¡Ahí no hay nada!"
conjunto (x:xs) = x: [a | a<-conjunto xs, a/=x]

-- CORREGIDO (Se quitó el caso base) - Obtener los números pares de una lista                  
numerosPares :: [Int] -> [Int]
numerosPares xs = [a | a<- xs, a `mod` 2 ==0]
