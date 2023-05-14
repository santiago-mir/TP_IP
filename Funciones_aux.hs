module Funciones_aux where

-- Funciones generales

-- Dada una lista, devuelve la misma lista dejando un único elemento por cada elemento distinto que tiene
quitarRepetidos :: (Eq t) => [t] -> [t]
quitarRepetidos [] = []
quitarRepetidos (x:xs) | longitud xs == 0 = [x]
                       | pertenece x xs = x : quitarRepetidos (quitarTodos x xs) 
                       | otherwise = x : quitarRepetidos xs
quitarTodos :: (Eq t ) => t -> [t] -> [t] -- Quitar todos los t de la lista [t], requiere que t esté en la lista
quitarTodos x xs | xs == [] || not (pertenece x xs) = xs
                 | otherwise = quitarTodos x (quitar x xs)
            
quitar :: (Eq t) => t -> [t] -> [t] -- Quitar el primer t de la lista [t], requiere que t esté en la lista
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
