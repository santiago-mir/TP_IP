module Tests where

import Test.HUnit
import Solucion
import Funciones_aux

-----------------------------------------------
-- Ejemplos
usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) 
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)
relacion4_5 = (usuario4, usuario5)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_1i = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2, publicacion2_1i]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3, relacion4_5]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

usuariosC = [usuario1, usuario2, usuario3, usuario4, usuario5]
relacionesC = [relacion1_2, relacion2_3, relacion3_4]
publicacionesC = [publicacion1_1, publicacion1_4]

redC = (usuariosC, relacionesC, publicacionesC)
--------------------------------------------------------------------------------------

run1 = runTestTT testSuiteEj1
run2 = runTestTT testSuiteEj2
run3 = runTestTT testSuiteEj3
run4 = runTestTT testSuiteEj4

redVacia = ([], [], [])
redSinRelaciones = (usuariosA, [], [])
red2 = ([usuario1], [], [])
red3 = (usuariosA, [], [])
red4 = (usuariosB, [], [])

testSuiteEj1 = test [
    "Caso 1: RedSocial sin usuarios" ~: (nombresDeUsuarios redVacia) ~?= [],
    "Caso 2: RedSocial con un solo usuario" ~: (nombresDeUsuarios red2) ~?= ["Juan"],
    "Caso 3: RedSocial con mas de un usuario" ~: (nombresDeUsuarios red3) ~?= ["Juan", "Natalia", "Pedro", "Mariela"],
    "Caso 4: RedSocial con usuarios del mismo nombre" ~: (nombresDeUsuarios red4) ~?= ["Juan", "Natalia", "Pedro"]
    ]

testSuiteEj2 = test [
    "Caso 1: RedSocial con usuarios, sin relaciones" ~: (amigosDe redSinRelaciones usuario1) ~?= [],
    "Caso 2: RedSocial con usuarios y relaciones, usuario sin amigos" ~: (amigosDe redC usuario5) ~?= [],
    "Caso 3: RedSocial con usuarios y relaciones, usuario con un amigo" ~: (amigosDe redC usuario1) ~?= [(2, "Natalia")],
    "Caso 4: RedSocial con usuarios y relaciones, usuario con mas de un amigo" ~: (amigosDe redA usuario2) ~?= [(1, "Juan"), (3, "Pedro"), (4, "Mariela")]
    ]

testSuiteEj3 = test [
    "Caso 1: RedSocial con usuarios, sin relaciones" ~: (cantidadDeAmigos redSinRelaciones usuario1) ~?= 0,
    "Caso 2: RedSocial con usuarios y relaciones, usuario sin amigos" ~: (cantidadDeAmigos redC usuario5) ~?= 0,
    "Caso 3: RedSocial con usuarios y relaciones, usuario con amigos" ~: (cantidadDeAmigos redA usuario2) ~?= 3
    ]      

testSuiteEj4 = test [
    "Red con varios usuarios, 2 con la mayor cantidad de amigos" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],
    "Red con usuarios, sin amigos" ~: expectAny (usuarioConMasAmigos redSinRelaciones) usuariosA,
    -- "Red sin usuarios" ~: expectAny (usuarioConMasAmigos redVacia) [], No se ve porque se requiere que haya usuarios
    "Red con un único usuario" ~: (usuarioConMasAmigos red2) ~?= usuario1
    -- Ejemplo de test: "descripcion" ~: (Implementacion.usuarioConMasAmigos red) ~?= usuario
    -- Ejemplo de test: "descripcion" ~: expectAny (Implementacion.usuarioConMasAmigos red) [ListaDePosiblesUsuarios],
    ] 
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)