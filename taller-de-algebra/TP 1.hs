{- Funciones auxiliares del Ejercicio 1, 2 y 3
esUnCubo fue dada en la consigna y chequea si un número es cubo
raizCubica calcula la raiz cubica
-}

esUnCubo :: Integer -> Bool
esUnCubo x = (round (fromIntegral x ** (1/3))) ^ 3 == x

raizCubica :: Integer -> Integer
raizCubica x = round (fromIntegral x ** (1/3))

{-Ejercicio 1:
esSumaDeDosCubosAux recibe el resultado r a chequear con un n1 que se va incrementando desde 1 y luego chequea si este r - (n1^3)
es también un cubo (ya sabe que n1^3 si o sí es cubo).
Si luego de restarle un número cubo, el resultante también es un cubo, devolverá True, en caso contrario, pasa a la segunda guarda.
Aquí el n1 se irá incrementando recursivamente hasta que el n1 sea menor o igual a la raiz cubica de r. Esto es porque si un
número es suma de dos cubos, ninguno de los sumandos será mayor a la raíz cúbica del mismo.

esSumaDeDosCubos pasa a la función auxiliar el n del output (el resultado r) y un n1=1 para que luego se vaya incrementando en
la auxiliar.
Además realiza el caso de n=1 con pattern matching, pues de otra manera siempre 1-(1^3) dará True ya que tomará que 0 es un cubo.
-}

esSumaDeDosCubosAux :: Integer -> Integer -> Bool
esSumaDeDosCubosAux n1 r | esUnCubo (r-(n1^3)) = True
                         | n1 <= raizCubica r = esSumaDeDosCubosAux (n1+1) r
                         | otherwise = False

esSumaDeDosCubos :: Integer -> Bool
esSumaDeDosCubos 1 = False
esSumaDeDosCubos n = esSumaDeDosCubosAux 1 n

{-Ejercicio 2:
descomposicionCubosAux usa la misma lógica que el ejercicio 1 pero en vez de devolver True, devuelve el n1 en cuestión (que ya 
sabemos que elevado al cubo síempre será cubo) y un n2 (que es la raiz cubica del r-(n1^3)) donde previamente chequea si es un cubo.
Caso contrario, pasa a la segunda guarda como en el primer ejercicio.

descomposicionCubos también sigue la misma lógica que el ejericicio anterior.
-}

descomposicionCubosAux :: Integer -> Integer -> (Integer, Integer)
descomposicionCubosAux n1 r | esUnCubo (r-(n1^3)) = (n1 , n2)
                            | n1 <= raizCubica r = descomposicionCubosAux (n1+1) r
                            | n1 > raizCubica r = undefined
                            where n2 = raizCubica (r-(n1^3))

descomposicionCubos :: Integer -> (Integer,Integer)
descomposicionCubos 1 = undefined
descomposicionCubos n = descomposicionCubosAux 1 n

{-Ejercicio 3:
cantidadDeFormasAux toma como caso base al n1 > raizCubica (r-(n1^3)) pues nos dimos cuenta probando distintos números
(ej: 4104 = 2^3 + 16^3 = 9^3 + 15^3) que a veces contaba de más y a veces de menos.
Una solución que encontramos ante esto es que mientras el n1 sea mayor al n2 la función se ejecutará y de esta manera
nos aseguramos de que no cuente 2 veces lo mismo.
Luego, usan la misma lógica de los ejercicios anteriores pero en vez de devolver True, suma 1 y en vez de devolver False,
suma 0. Después continúa la recursión en ambos casos.

cantidadDeFormas llama a la función auxiliar tomando como n1=1 para que luego se siga incrementando
-}

cantidadDeFormasAux :: Integer -> Integer -> Integer
cantidadDeFormasAux n1 r | n1 > n2 = 0
                         | esUnCubo (r-(n1^3)) = 1 + (cantidadDeFormasAux (n1+1) r)
                         | not (esUnCubo (r-(n1^3))) = 0 + (cantidadDeFormasAux (n1+1) r)
                         where n2 = raizCubica (r-(n1^3))

cantidadDeFormas :: Integer -> Integer
cantidadDeFormas n = cantidadDeFormasAux 1 n

{-Ejercicio 4:
esEspecial chequea si la cantidad de Formas es mayor o igual a 2.

especialDesde devuelve n si el número que le pasamos es especial, y si no, pasa a la otra guarda en donde
prueba con el siguiente (se sigue ejecutando hasta encontrar alguno ya que hay infinitos números especiales).
-}

esEspecial :: Integer -> Bool
esEspecial n | cantidadDeFormas n >= 2 = True
             | otherwise = False

especialDesde :: Integer -> Integer
especialDesde n | esEspecial n = n
                | otherwise = especialDesde (n+1) 

{-Ejercicio 5:
Sabemos que el nesimo especial es el siguiente al (n-1) especial y tomamos como caso base al 1729 por ser el primer especial.
Nos pareció análogo al pensamiento del nésimo primo de la teórica de la clase 5.
-}

especialNumero :: Integer -> Integer
especialNumero n | n == 1 = 1729
                 | otherwise = especialDesde (1 + especialNumero (n-1))

{-Ejercicio 6:
esDivisorYCubo chequea si d es divisor de n y a su vez si d es un cubo.

noHayDivisorCubo chequea si hay algún divisor que deje como cociente a un número especial (en ese caso
no es muy especial entonces devuelve False) y se detiene en caso de que el divisor itinerante sea mayor
al dividendo.

esMuyEspecial toma el resultado de la función auxiliar anterior usando como divisor itinerante base al 8
pues es el primer divisor cubo posible en el caso de que el n sea especial. Si no lo fuese, directamente
devuelve False.
-}

esDivisorYCubo :: Integer -> Integer -> Bool
esDivisorYCubo d n | (esUnCubo d) && (mod n d == 0) = True
                   | otherwise = False

noHayDivisorCubo :: Integer -> Integer -> Bool
noHayDivisorCubo i n | i >= n = True
                     | (esDivisorYCubo i n) && esEspecial (div n i) = False
                     | otherwise = noHayDivisorCubo (1+i) n

esMuyEspecial :: Integer -> Bool
esMuyEspecial n | esEspecial n = (noHayDivisorCubo 8 n)
                | otherwise = False
