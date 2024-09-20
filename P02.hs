-----------LISTAS Y RECURSIÓN-------------

--Longitud de una lista
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--Suma de n números
sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

--Agregar elemento a una lista
agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento [] a bool = a:[] --Aquí es indistinto colocar el elemento por delante o por detrás pues la lista está vacía, terminará igual.
agregaElemento (x:xs) a bool =
  if bool
  then a:(x:xs)
  else (x:xs)++[a]

-- Máximo de una lista
maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [] = error "¡Ahí no hay nada!"
maximoLista [a] = a
maximoLista (x:xs) =

  if x>maximoLista xs
  then x
  else maximoLista xs

--Recuperar un elemento de una lista de acuerdo a su índice (con comienzo en 0)
indice :: [a] -> Int -> a
indice [] int = error "¡No hay nada que extraer!"
indice [a] int = a
indice (x:xs) 0 = x
indice (x:xs) int =

  if int>=0 && int<= longitud (x:xs)-1  
  then indice xs
       (int-1)
  else error "No hay ningún elemento bajo tal índice"

-- Recuperar un elemento de una lista de acuerdo a su índice (de manera intuitiva, con comienzo en 1)
indiceI :: [a] -> Int -> a
indiceI [] int = error "¡No hay nada que extraer!"
indiceI [a] int = a
indiceI (x:xs) 0 = error "¡No existe tal posición!"
indiceI (x:xs) 1 = x
indiceI (x:xs) int =

  if int>=0 && int<= longitud (x:xs)  
  then indiceI xs
       (int-1)
  else error "No hay ningún elemento bajo tal índice"

-----------LISTAS POR COMPRENSIÓN Y RECURSIÓN POR COLA-----------

-- Divisores de un número entero
divisores :: Int -> [Int]
divisores 0 = error "El 0 tiene infinitos divisores"
divisores int = [i | i<-[1..int],int `mod `i ==0]
  
-- Convertir una lista en conjunto
conjunto :: Eq a => [a] -> [a]
conjunto [a] = [a]
conjunto [] = error "¡Ahí no hay nada!"
conjunto (x:xs) = x: [a | a<-conjunto xs, a/=x]

-- Obtener los números pares de una lista                  
numerosPares :: [Int] -> [Int]
numerosPares [] = error "¡Ahí no hay nada!"
numerosPares xs = [a | a<- xs, a `mod` 2 ==0]
