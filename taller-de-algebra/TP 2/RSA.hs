module RSA where
import Tipos
import Aritmetica


-- (3)
--e seguro funciona si es coprimo con m. no me preocupo por los casos chicos pues p y q son por lo menos 11
--d puede ser su inverso multiplicativo que existe siempre pues e y m coprimos

--puedo usar la funcion coprimoCon o agarrar directo el inmediato anterior pues siempre va a ser coprimo
--si hago eso me va a dar una congruencia con -1 y e=d
claves :: Integer -> Integer -> (Integer, Integer, Integer)
claves p q = (e, d, n)
    where e = coprimoCon ((p-1)*(q-1))
          d = inversoMultiplicativoPositivo e ((p-1)*(q-1))
          n = p*q

--clave publica (e, n)
--clave privada (d, n)

-- (6)
codificadorConEnteros:: Clpub -> [Integer] -> Cifrado
codificadorConEnteros _ [] = []
codificadorConEnteros (e,n) (m:ms) | mcd n m == 1 = encriptacion1 : (codificadorConEnteros (e,n) ms)
                                   | otherwise = encriptacion2 : (codificadorConEnteros (e,n) ms)
                    where encriptacion1 = modExp m e n -- mod (m^e) n
                          encriptacion2 = -m

codificador :: Clpub -> Mensaje -> Cifrado
codificador (e,n) mensaje = codificadorConEnteros (e,n) (aEnteros mensaje)

-- (7)
decodificadorPrincipal:: Clpri -> Cifrado -> [Integer]
decodificadorPrincipal _ [] = []
decodificadorPrincipal (d,n) (c:cs) | (c >= 0) = decodificacion1 : (decodificadorPrincipal (d,n) cs)
                                    | otherwise = decodificacion2 : (decodificadorPrincipal (d,n) cs)
                    where decodificacion1 = modExp c d n --mod (c^d) n 
                          decodificacion2 = -c

decodificador :: Clpri -> Cifrado -> Mensaje
decodificador (d,n) c = aChars (decodificadorPrincipal (d,n) c)
