module Funciones_aux where

-- Funciones generales

-- Dada una lista, devuelve la misma lista dejando un único elemento por cada elemento distinto que tiene
quitarRepetidos :: (Eq t) => [t] -> [t]
quitarRepetidos [] = []
quitarRepetidos (x:xs) | longitud xs == 0 = [x]
                       | pertenece x xs = x : quitarRepetidos (quitarTodos x xs) 
                       | otherwise = x : quitarRepetidos xs

-- Quitar todos los t de la lista [t], requiere que t esté en la lista
quitarTodos :: (Eq t ) => t -> [t] -> [t]
quitarTodos x xs | xs == [] || not (pertenece x xs) = xs
                 | otherwise = quitarTodos x (quitar x xs)

-- Quitar el primer t de la lista [t], requiere que t esté en la lista            
quitar :: (Eq t) => t -> [t] -> [t]
quitar n (x:xs) | n == x = xs
                | otherwise = x : quitar n xs

-- Dada una lista, devuelve la longitud de la misma
longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- Dado un elemento y una lista del mismo tipo T, determina si el elemento pertenece a la lista
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece x (y:ys) = x == y || pertenece x ys

--
cantidad :: (Eq t) => t -> [t] -> Int
cantidad _ [] = 0
cantidad x (y:ys)
    | x == y = 1 + cantidad x ys
    | otherwise = cantidad x ys

--
sonPermutacion :: (Eq t) => [t] -> [t] -> Bool 
sonPermutacion list1 list2 = longitud list1 == longitud list2 && mismaCantidadDeCadaElemento list1 list2
        where mismaCantidadDeCadaElemento :: (Eq t) => [t] -> [t] -> Bool
              mismaCantidadDeCadaElemento [] _ = True
              mismaCantidadDeCadaElemento (x:xs) ys = cantidad x (x:xs) == cantidad x ys && mismaCantidadDeCadaElemento (quitarTodos x (x:xs)) ys
