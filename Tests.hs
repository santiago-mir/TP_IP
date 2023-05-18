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
usuario6 = (6, "Roberto")
usuario7 = (7, "María")
usuario8 = (8, "Luis")
usuario9 = (9, "Don José")
usuario10 = (10, "Lucía")
usuario11 = (11, "Alex")
usuario12 = (12, "Alex")


relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) 
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)
relacion4_5 = (usuario4, usuario5)
relacion1_5 = (usuario1, usuario5)
relacion1_6 = (usuario1, usuario6)
relacion1_7 = (usuario1, usuario7)
relacion1_8 = (usuario1, usuario8)
relacion1_9 = (usuario1, usuario9)
relacion1_10 = (usuario1, usuario10)
relacion1_11 = (usuario1, usuario11)
relacion1_12 = (usuario1, usuario12)

relacion2_5 = (usuario2, usuario5)
relacion3_5 = (usuario3, usuario5)
relacion6_7 = (usuario6, usuario7)
relacion6_8 = (usuario6, usuario8)
relacion7_8 = (usuario8, usuario7)
relacion9_10 = (usuario9, usuario10)
relacion10_11 = (usuario10, usuario11)
relacion11_12 = (usuario11, usuario12)

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

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)
esPermutacion actual expected = sonPermutacion actual expected ~? ("expected permutacion of: " ++ show expected ++ "\n but got: " ++ show actual)

run1 = runTestTT testSuiteEj1
run2 = runTestTT testSuiteEj2
run3 = runTestTT testSuiteEj3
run4 = runTestTT testSuiteEj4
run5 = runTestTT testSuiteEj5
run6 = runTestTT testSuiteEj6
run7 = runTestTT testSuiteEj7
-- run8 = runTestTT testSuiteEj8
-- run9 = runTestTT testSuiteEj9
-- run10 = runTestTT testSuiteEj10


-- todosTests = [testSuiteEj1, testSuiteEj2, testSuiteEj3, testSuiteEj4, testSuiteEj5, testSuiteEj6, testSuiteEj7, testSuiteEj8, testSuiteEj9, testSuiteEj10] 

usuariosMuchos = [usuario1,usuario2,usuario3,usuario4,usuario5,usuario6,usuario7,usuario8,usuario9,usuario10,usuario11,usuario12]
relacionesMuchas = [relacion1_2,relacion1_3,relacion1_4,relacion2_3,relacion2_4,relacion3_4,relacion4_5,
    relacion1_5,relacion1_6,relacion1_7,relacion1_8,relacion1_9, relacion1_10,relacion1_11,relacion1_12]

redVacia = ([], [], [])
redSinRelaciones = (usuariosA, [], [])
red2 = ([usuario1], [], [])
red3 = (usuariosA, [], [])
red4 = (usuariosB, [], [])
red5 = (usuariosMuchos, [relacion1_2, relacion2_3, relacion3_4, relacion4_5], [])
red6 = (usuariosMuchos, [relacion1_2, relacion2_3, relacion3_4, relacion4_5, relacion3_5, relacion2_5, relacion1_3,
    relacion6_8, relacion9_10, relacion7_8, relacion6_7, relacion11_12, relacion10_11], [])
redRobertoCarlos = (usuariosMuchos, relacionesMuchas, publicacionesA)

-- nombresDeUsuarios
testSuiteEj1 = test [
    -- las relaciones y publicaciones no tienen relevancia en este ejercicio, no se toman en cuenta
    "Caso 1: RedSocial sin usuarios" ~: (nombresDeUsuarios redVacia) ~?= [],
    "Caso 2: RedSocial con un solo usuario" ~: (nombresDeUsuarios red2) ~?= ["Juan"],
    "Caso 3: RedSocial con mas de un usuario" ~: (nombresDeUsuarios red3) ~?= ["Juan", "Natalia", "Pedro", "Mariela"],
    "Caso 4: RedSocial con usuarios del mismo nombre" ~: (nombresDeUsuarios red4) ~?= ["Juan", "Natalia", "Pedro"]
    ]

-- amigosDe
testSuiteEj2 = test [
    -- El caso de red social vacia no aplica, la especificacion pide que el usuario pertenezca a la red.
    -- Las publicaciones no tienen relevancia en este ejercicio, no se tienen en cuenta
    "Caso 1: RedSocial con un usuario, sin relaciones" ~: (amigosDe red2 usuario1) ~?= [],
    "Caso 2: RedSocial con mas de un usuario, sin relaciones" ~: (amigosDe red3 usuario1) ~?= [],
    "Caso 3: RedSocial con mas de un usuario y relaciones, usuario sin amigos" ~: (amigosDe redC usuario5) ~?= [],
    "Caso 3: RedSocial con mas de un usuario y relaciones, usuario con un amigo" ~: (amigosDe redC usuario1) ~?= [(2, "Natalia")],
    "Caso 4: RedSocial con mas de un usuario y relaciones, usuario con mas de un amigo" ~: (amigosDe redA usuario2) ~?= [(1, "Juan"), (3, "Pedro"), (4, "Mariela")]
    ]

-- cantidadDeAmigos
testSuiteEj3 = test [
    -- El caso de red social vacia no aplica, la especificacion pide que el usuario pertenezca a la red.
    -- Las publicaciones no tienen relevancia en este ejercicio, no se tienen en cuenta
    "Caso 1: RedSocial con usuarios, sin relaciones" ~: (cantidadDeAmigos redSinRelaciones usuario1) ~?= 0,
    "Caso 2: RedSocial con usuarios y relaciones, usuario sin amigos" ~: (cantidadDeAmigos redC usuario5) ~?= 0,
    "Caso 3: RedSocial con usuarios y relaciones, usuario con amigos" ~: (cantidadDeAmigos redA usuario2) ~?= 3
    ]      

-- usuarioConMasAmigos
testSuiteEj4 = test [
    -- El caso de red social vacia no aplica, la especificacion pide que la red social tenga por lo menos un usuario
    -- Las publicaciones no tienen relevancia en este ejercicio, no se tienen en cuenta
    "Caso 1: RedSocial con un único usuario, sin relaciones" ~: (usuarioConMasAmigos red2) ~?= usuario1,
    "Caso 2: RedSocial con mas de un usuario, sin relaciones" ~: expectAny (usuarioConMasAmigos redSinRelaciones) usuariosA,
    "Caso 3: RedSocial con mas de un usuario, 2 con la mayor cantidad de amigos" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],
    "Caso 4: RedSocial con mas de un usuario, usuario2 con mas amigos que el resto" ~: (usuarioConMasAmigos redB) ~?= usuario2
    ] 

-- -- estaRobertoCarlos
testSuiteEj5 = test [
    -- Las publicaciones no tienen relevancia en este ejercicio, no se tienen en cuenta
    "Caso 1: RedSocial sin usuarios ni relaciones" ~: (estaRobertoCarlos redVacia) ~?= False,
    "Caso 2: RedSocial con un unico usuario, sin relaciones" ~: (estaRobertoCarlos red2) ~?= False,
    "Caso 3: RedSocial con mas de un usuario, sin relaciones" ~: (estaRobertoCarlos red3) ~?= False,
    "Caso 4: RedSocial con mas de un usuario, pero ninguno con más de 10 amigos" ~: (estaRobertoCarlos redA) ~?= False,
    "Caso 5: RedSocial con mas de un usuario, alguno con más de 10 amigos" ~: (estaRobertoCarlos redRobertoCarlos) ~?= True
    ]

-- -- publicacionesDe
testSuiteEj6 = test [
    -- El caso de red social vacia no aplica, la especificacion pide que la red social tenga por lo menos un usuario
    -- Las relaciones no tienen relevancia en este ejercicio, no se tienen en cuenta
    "Caso 1: RedSocial con un unico usuario, sin publicaciones" ~: (publicacionesDe red2 usuario1) ~?= [],
    "Caso 2: RedSocial con mas de un usuario, sin publicaciones" ~: (publicacionesDe red3 usuario1) ~?= [],
    "Caso 3: RedSocial con mas de un usuario y publicaciones, usuario5 sin publicaciones" ~: (publicacionesDe redC usuario5) ~?= [],
    "Caso 4: RedSocial con mas de un usuario y publicaciones, usuario1 con publicaciones" ~: (publicacionesDe redA usuario1) ~?= [publicacion1_1, publicacion1_2]  
    --  Que hace este test? " publicacionesDe 1" ~: esPermutacion (publicacionesDe redA usuario2) [publicacion2_1, publicacion2_2],
    ]

-- -- publicacionesQueLeGustanA
testSuiteEj7 = test [
    -- El caso de red social vacia no aplica, la especificacion pide que la red social tenga por lo menos un usuario
    "Caso 1: RedSocial con un unico usuario, sin publicaciones" ~: (publicacionesQueLeGustanA red2 usuario1) ~?= [],
    "Caso 2: RedSocial con mas de un usuario, sin publicaciones" ~: (publicacionesQueLeGustanA red3 usuario1) ~?= [],
    "Caso 3: RedSocial con mas de un usuario y publicaciones, usuario sin publicaciones likeadas" ~: (publicacionesQueLeGustanA redB usuario3) ~?= [],
    "Caso 4: RedSocial con mas de un usuario y publicaciones, usuario con una publicacion likeada" ~: (publicacionesQueLeGustanA redC usuario2) ~?= [publicacion1_1],
    "Caso 5: RedSocial con mas de un usuario y publicaciones, usuario con mas de una publicacion likeada" ~: (publicacionesQueLeGustanA redA usuario2) ~?= [publicacion1_1, publicacion3_2, publicacion4_1],
    " publicacionesQueLeGustanA 1" ~: esPermutacion (publicacionesQueLeGustanA redA usuario1) [publicacion2_2, publicacion4_1]
    ]

-- -- lesGustanLasMismasPublicaciones
-- testSuiteEj8 = test [
--     " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True
--     ]

-- -- tieneUnSeguidorFiel
-- testSuiteEj9 = test [
--     " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True
--     ]

-- -- existeSecuenciaDeAmigos
-- testSuiteEj10 = test [
--     "Red con usuarios, con los usuarios relacionados en primer orden (directamente)" ~: (existeSecuenciaDeAmigos redA usuario1 usuario2) ~?= True,
--     "Red con usuarios, con los usuarios relacionados en segundo orden (desde un solo amigo)" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True,
--     "Red con usuarios, con los usuarios relacionados en tercer orden (desde dos amigos)" ~: (existeSecuenciaDeAmigos red5 usuario1 usuario5) ~?= True,
--     "Red con usuarios, siendo ambos usuarios el mismo, tiene amigos" ~: (existeSecuenciaDeAmigos redA usuario1 usuario1) ~?= True,
--     "Red con usuarios, siendo ambos usuarios el mismo, no tiene amigos" ~: (existeSecuenciaDeAmigos redA usuario5 usuario5) ~?= False,
--     "Red con usuarios, todos con amigos pero sin existir una cadena" ~: (existeSecuenciaDeAmigos red6 usuario1 usuario10) ~?= False,
--     " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True
--     ]
