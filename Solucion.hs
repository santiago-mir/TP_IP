module Solucion where
import Funciones_aux

-- Completar con los datos del grupo
--
-- Nombre de Grupo: Grupo.hs
-- Integrante 1: Martín Dosio, dosiomartin@gmail.com, 291/23
-- Integrante 2: Santiago Miranda, san_chan97@hotmail.com, 418/18
-- Integrante 3: Lucas Pujia, lucas.pujia@gmail.com, 481/23
-- Integrante 4: Luna Praino, prainolunaa@gmail.com, 77/23

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

              -- Devuelve los nombres de usuario de los usuarios que están en la red social
        where nombresDeUsuariosAux :: RedSocial -> [String]   
              nombresDeUsuariosAux ([], _, _) = []
              nombresDeUsuariosAux (u:us, rs, ps) | longitud us == 0 = [nombreDeUsuario u]
                                                  | pertenece u us = nombresDeUsuarios (us, rs, ps)
                                                  | otherwise = nombreDeUsuario u : nombresDeUsuarios (us, rs, ps)

-- Ejercicio 2

-- Dado un usuario, devuelve una lista con sus amigos
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (us,rs,ps) user = quitarRepetidos (amigosDeAux rs user)

              -- Recibe una lista de relaciones y un usuario y devuelve una lista con los amigos de ese usuario en la lista de relaciones
        where amigosDeAux :: [Relacion] -> Usuario -> [Usuario]
              amigosDeAux [] _ = []
              amigosDeAux (r:rel) user | user1 == user = user2 : amigosDeAux rel user
                                       | user2 == user = user1 : amigosDeAux rel user
                                       | otherwise = amigosDeAux rel user
                                        where user1 = fst r
                                              user2 = snd r

-- Ejercicio 3

-- Dado un usuario, devuelve la cantidad de amigos que tiene
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos rs us = fromIntegral (longitud (amigosDe rs us))

-- Ejercicio 4

-- Dada una red válida con al menos un usuario, devuelve el usuario con más amigos
usuarioConMasAmigos :: RedSocial -> Usuario     -- Se fija entre los primeros 2 usuarios cuál tiene más amigos, y repite con el resto de los usuarios
usuarioConMasAmigos ([user], _, _) = user       -- y el que tenga más de estos. Cuando solo quede un usuario, ese será el que tenga más amigos
usuarioConMasAmigos (user1:user2:us, rs, ps) | longitud us == 0 && cantidadDeAmigos red user1 >= cantidadDeAmigos red user2 = user1
                                             | longitud us == 0 && cantidadDeAmigos red user2 > cantidadDeAmigos red user1 = user2
                                             | cantidadDeAmigos red user1 >= cantidadDeAmigos red user2 = usuarioConMasAmigos (user1:us,rs,ps)
                                             | otherwise = usuarioConMasAmigos (user2:us,rs,ps)
                                             where red = (user1:user2:us, rs, ps)

-- Ejercicio 5

-- Dada una red social, busca si existe un usuario que tenga más de 10 amigos
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([], _, _) = False
estaRobertoCarlos red = cantidadDeAmigos red (usuarioConMasAmigos red) > 10

-- Ejercicio 6

-- Dado un usuario, devuelve las publicaciones que subió
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (_, _, []) _ = []
publicacionesDe red user = quitarRepetidos (publicacionesDeAux red user)

              -- Verifica si la publicación en la cabeza de la lista es del usuario pasado como argumento
        where publicacionesDeAux :: RedSocial -> Usuario -> [Publicacion]
              publicacionesDeAux (us, rs, p:ps) user | longitud ps == 0 && usuarioDePublicacion p == user = [p]
                                                     | longitud ps == 0 && (usuarioDePublicacion p /= user) = []
                                                     | usuarioDePublicacion p == user = p : publicacionesDeAux (us, rs, ps) user
                                                     | otherwise = publicacionesDeAux (us, rs, ps) user

-- Ejercicio 7

-- Dada una red social y un usuario, devuelve las publicaciones a las que le dio like
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (us, rs, p:ps) user | longitud ps == 0 && not (pertenece user (likesDePublicacion p)) = []
                                              | longitud ps == 0 && pertenece user (likesDePublicacion p) = [p]
                                              | pertenece user (likesDePublicacion p) = p : publicacionesQueLeGustanA (us, rs, ps) user
                                              | otherwise = publicacionesQueLeGustanA (us, rs, ps) user

-- Ejercicio 8

-- Dada una red social y dos usuarios, verifica si les gustan las mismas publicaciones
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = sonPermutacion (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)
-- Como pueden estar en orden diferente un == no alcanza, y por eso hay que verificar que sean permutaciones entre sí

-- Ejercicio 9

-- Dada una red social y un usuario verifica si existe un usuario que le haya dado like a todas las publicaciones de otro usuario, que tienen que ser al menos una
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel r u | longitud (publicacionesDe r u) == 0 = False
                        | otherwise = tieneUnSeguidorFielAux2 (tail (publicacionesDe r u)) (likesDePublicacion (head (publicacionesDe r u)))

              -- Verifica si alguno de los usuarios de la lista de likes de la primera publicación es un seguidor fiel
        where tieneUnSeguidorFielAux2 :: [Publicacion] -> [Usuario] -> Bool
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

-- Dados dos usuarios, determina si existe una secuencia de amigos que los conecte
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red usuario1 usuario2 = existeSecuenciaDeAmigosEntreSusAmigos red usuario1 usuario2 []

               -- Busca si existe una secuencia de amigos que conecte dos usuarios en la red social
        where  existeSecuenciaDeAmigosEntreSusAmigos :: RedSocial -> Usuario -> Usuario -> [Usuario] -> Bool
               existeSecuenciaDeAmigosEntreSusAmigos red user1 user2 repetidos | pertenece user1 repetidos = False
                                                                               | pertenece (user1, user2) (relaciones red) || pertenece (user2, user1) (relaciones red) = True
                                                                               | otherwise = anadirAmigoARepetidosYRepetir (amigosDe red user1)

                      -- Verifica si existe una secuencia de amigos entre los amigos de los usuarios en la lista 'us' que incluya tanto al usuario 'user1' como al usuario 'user2'
                where anadirAmigoARepetidosYRepetir :: [Usuario] -> Bool
                      anadirAmigoARepetidosYRepetir [] = False
                      anadirAmigoARepetidosYRepetir (u:us) | u == user1 = False
                                                           | otherwise = existeSecuenciaDeAmigosEntreSusAmigos red u user2 (user1:repetidos) || anadirAmigoARepetidosYRepetir us
