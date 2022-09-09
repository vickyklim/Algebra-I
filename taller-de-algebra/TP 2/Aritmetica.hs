module Aritmetica where
import Tipos
import Data.Tuple
import Data.Bits


-- (1)
mcd :: Integer -> Integer -> Integer
mcd a 0 = abs a
mcd a b = mcd (min a b) (mod (max a b) (min a b))

euclides :: Integer -> Integer -> (Integer, Integer, Integer)
euclides a 0 = (abs a, 1, 0)
euclides a b = (d, t', s' - a `div` b * t')
    where (d, s', t') = euclides b (a `mod` b)

primerElem3upla:: (Integer, Integer, Integer) -> Integer
primerElem3upla (a, b, c) = a

segundoElem3upla:: (Integer, Integer, Integer) -> Integer
segundoElem3upla (a, b, c) = b

tercerElem3upla:: (Integer, Integer, Integer) -> Integer
tercerElem3upla (a, b, c) = c

mcdExt :: Integer -> Integer -> (Integer, (Integer, Integer))
mcdExt a b = (primerElem3upla e, (segundoElem3upla e, tercerElem3upla e))
    where e = euclides a b

-- (2)

menorDivisorDesde:: Integer -> Integer -> Integer
menorDivisorDesde n k | mod n k == 0 = k
                      | k > (div n 2) = n
                      | otherwise = menorDivisorDesde n (k+1)

menorDivisor:: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2 

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = (n == (menorDivisor n))

criba :: Integer -> Set Integer
criba 1 = []
criba n | not (esPrimo (n-1)) = criba (n-1)
        | esPrimo (n-1) = (n-1) : (criba (n-1))

-- (3)
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = (mcd a b == 1)

coprimoConAux :: Integer -> Integer -> Integer
coprimoConAux n 1 = undefined
coprimoConAux n x | (sonCoprimos n x) = x
                  | otherwise = coprimoConAux n (x-1)

coprimoCon:: Integer -> Integer
coprimoCon n = coprimoConAux n (n-2)

-- (4)

--asumo que son coprimos
inversoMultiplicativo:: Integer -> Integer -> Integer
inversoMultiplicativo n m  = segundoElem3upla (euclides n m)

--le voy sumando n porque la diofántica es s*n+t*m=d con (s=x0 + mk) con k entero
inversoMultiplicativoPositivo:: Integer -> Integer -> Integer
inversoMultiplicativoPositivo n m | (inversoMultiplicativo n m) > 0 = inversoMultiplicativo n m
                                  | otherwise = hacerloPositivo (inversoMultiplicativo n m) m

hacerloPositivo:: Integer -> Integer -> Integer
hacerloPositivo s m | s > 0 = s
                    | otherwise = hacerloPositivo (s + m) m 

-- Función de regalo para exponenciar "rápido"
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1
