module Solucion where
import Funciones_aux

-- Completar con los datos del grupo
--
-- Nombre de Grupo: Grupo.hs
-- Integrante 1: Martìn Dosio, dosiomartin@gmail.com, 291/23
-- Integrante 2: Santiago Miranda, san_chan97@hotmail.com, 418/18
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])


-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

-- Ejercicio 1

-- Recorre la lista de usuarios de la red social y toma los nombres de cada usuario

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = quitarRepetidos (nombresDeUsuariosAux red)

        where   nombresDeUsuariosAux :: RedSocial -> [String]   -- Sería la función principal, solo que puede contener repetidos.
                nombresDeUsuariosAux ([], _, _) = []            -- Recorre la lista de usuarios y los va agregando al resultado
                nombresDeUsuariosAux ((u:us), rs, ps)| longitud us == 0 = [nombreDeUsuario u]
                                                | pertenece u us   = nombresDeUsuarios (us, rs, ps)
                                                | otherwise        = nombreDeUsuario u : nombresDeUsuarios (us, rs, ps)

                                  
-- Ejercicio 2

-- Dado un usuario, devuelve una lista con sus amigos

amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (us,rs,ps) user = quitarRepetidos(amigosDeAux rs user)

        where   amigosDeAux :: [Relacion] -> Usuario -> [Usuario]
                amigosDeAux [] _ = []
                amigosDeAux (r:rel) user | user1 == user = user2 : amigosDeAux rel user
                                        | user2 == user = user1 : amigosDeAux rel user
                                        | otherwise = amigosDeAux rel user
                                        where   user1 = fst r
                                                user2 = snd r  

-- Ejercicio 3

-- Dado un usuario, devuelve la cantidad de amigos que tiene

cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos rs us = fromIntegral (longitud (amigosDe rs us))

-- Ejercicio 4
 
-- Dada una red válidoa con al menos un usuario, devuelve el usuario con más amigos

usuarioConMasAmigos :: RedSocial -> Usuario     -- Se fija entre los primeros 2 usuarios cuál tiene más amigos, y repite con el resto de los usuarios
usuarioConMasAmigos ([user], _, _) = user       -- y el que tenga más de estos. Cuando solo quede un usuario, ese será el que tenga más amigos
usuarioConMasAmigos ((user1:user2:us), rs, ps)
        | longitud us == 0 && cantidadDeAmigos red user1 >= cantidadDeAmigos red user2 = user1
        | longitud us == 0 && cantidadDeAmigos red user2 > cantidadDeAmigos red user1 = user2
        | cantidadDeAmigos red user1 >= cantidadDeAmigos red user2 = usuarioConMasAmigos (user1:us,rs,ps) 
        | otherwise = usuarioConMasAmigos (user2:us,rs,ps)
        where red = ((user1:user2:us), rs, ps)
              
-- Ejercicio 5
              
-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([], _, _) = False
estaRobertoCarlos red = cantidadDeAmigos red (usuarioConMasAmigos red) > 10
                                   
-- Ejercicio 6

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (us, rs, (p:ps)) user | longitud ps == 0 && usuarioDePublicacion p == user = [p]
                                      | longitud ps == 0 && not (usuarioDePublicacion p == user) = []
                                      | usuarioDePublicacion p == user = p : publicacionesDe (us, rs, ps) user
                                      | otherwise = publicacionesDe (us, rs, ps) user

-- Ejercicio 7

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (us, rs, (p:ps)) user | longitud ps == 0 && not (pertenece user (likesDePublicacion p)) = []  
                                                | longitud ps == 0 && pertenece user (likesDePublicacion p) = [p]
                                                | pertenece user (likesDePublicacion p) = p : publicacionesQueLeGustanA (us, rs, ps) user
                                                | otherwise                             = publicacionesQueLeGustanA (us, rs, ps) user                    

-- Ejercicio 8

-- Dada una red social y dos usuarios, verifica si les gustan las mismas publicaciones
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones r u1 u2 = publicacionesQueLeGustanA r u1 == publicacionesQueLeGustanA r u2

-- Ejercicio 9

-- Dada una red social y un usuario verifica si existe un usuario que le haya dado like a todas las publicaciones de otro usuario, que tienen que ser al menos una
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel r u | longitud (publicacionesDe r u) == 0 = False
                        | otherwise = tieneUnSeguidorFielAux2 (tail (publicacionesDe r u)) (likesDePublicacion (head (publicacionesDe r u)))

-- Verifica si alguno de los usuarios de la lista de likes de la primera publicación es un seguidor fiel
tieneUnSeguidorFielAux2 :: [Publicacion] -> [Usuario] -> Bool
tieneUnSeguidorFielAux2 _ [] = False
tieneUnSeguidorFielAux2 [] us = longitud us >= 1
tieneUnSeguidorFielAux2 p (u:us) | longitud us == 0 = tieneUnSeguidorFielAux p u
                                 | otherwise = tieneUnSeguidorFielAux p u || tieneUnSeguidorFielAux2 p us

-- Verifica si un usuario ha dado like a todas las publicaciones de 'u'
tieneUnSeguidorFielAux :: [Publicacion] -> Usuario -> Bool
tieneUnSeguidorFielAux (p:ps) u | longitud ps == 0 = pertenece u (likesDePublicacion p)
                                | pertenece u (likesDePublicacion p) = tieneUnSeguidorFielAux ps u
                                | otherwise = False

-- Ejercicio 10

-- describir qué hace la función: .....
          
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red usuario1 usuario2 = existeSecuenciaDeAmigosEntreSusAmigos red usuario1 usuario2 []

    where   existeSecuenciaDeAmigosEntreSusAmigos :: RedSocial -> Usuario -> Usuario -> [Usuario] -> Bool
            existeSecuenciaDeAmigosEntreSusAmigos red user1 user2 repetidos
                | pertenece user1 repetidos = False
                | pertenece (user1, user2) (relaciones red) || pertenece (user2, user1) (relaciones red) = True
                | otherwise = anadirAmigoARepetidosYRepetir (amigosDe red user1)

                where   anadirAmigoARepetidosYRepetir :: [Usuario] -> Bool
                        anadirAmigoARepetidosYRepetir [] = False
                        anadirAmigoARepetidosYRepetir (u:us)
                            | u == user1 = False
                            | otherwise = existeSecuenciaDeAmigosEntreSusAmigos red u user2 (user1:repetidos) || anadirAmigoARepetidosYRepetir us
