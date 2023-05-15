module Solucion where
import Funciones_aux

-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
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


-- Recorre la lista de usuarios de la red social y toma los nombres de cada usuario

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = quitarRepetidos (nombresDeUsuariosAux red)

        where   nombresDeUsuariosAux :: RedSocial -> [String]
                nombresDeUsuariosAux ([], _, _) = []     
                nombresDeUsuariosAux ((u:us), rs, ps)| longitud us == 0 = [nombreDeUsuario u]
                                                | pertenece u us   = nombresDeUsuarios (us, rs, ps)
                                                | otherwise        = nombreDeUsuario u : nombresDeUsuarios (us, rs, ps)

                                  


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


-- Dado un usuario, devuelve la cantidad de amigos que tiene

cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos rs us = fromIntegral (longitud (amigosDe rs us))

 
-- describir qué hace la función: .....

usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ((u:us), rs, ps) = undefined
                                     
-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ((u:us), rs, ps) = undefined
                                   

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (us, rs, (p:ps)) user = undefined

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (us, rs, (p:ps)) user = undefined                        

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones rs u1 u2 = undefined

-- describir qué hace la función: .....

tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel ((u:us), rs, ps) user = undefined

-- describir qué hace la función: .....

                                                   
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2  = undefined