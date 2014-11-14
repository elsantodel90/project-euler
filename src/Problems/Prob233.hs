module Problems.Prob233 where

{-
Si n = 2k   es par  , f n = 4 * prod(b + 1), siendo los b los exponentes de los primos 4k+1 en la fact de 2k^2 = n^2 / 2
Si n = 2k+1 es impar, f n = 4 * prod(b + 1), siendo los b los exponentes de los primos 4k+1 en la fact de 4k(k+1) + 1 = n^2

*** En todos los casos, f n = 4 * prod(2*b+1), siendo b los exponentes de los primos 4k+1 en la fact de n ***
(Ver "cuantas soluciones enteras tiene a^2 + b^2 = k", para un entero k, y lo mismo a(a+1) + b(b+1) = k)
                  
420 = 4 * 3 * 5 * 7. De ahi prod(2b+1) = 3 * 5 * 7. Luego la "parte esencial" de n solo puede tener las siguientes pintas:
p * q^2 * r^3
p^3 * q^7
p^2 * q^10
p * q^17 --> Necesariamente mayor a 10^11
p^52     --> Necesariamente mayor a 10^11
Con p,q,r primos de la forma 4k+1
Una vez que encontramos una parte esencial de n valida, hay que buscar sus multiplos no divisibles por un primo 4k+1,
y con la parte de los 4k+3 un cuadrado. Esta parte no puede ser mayor a 5228758 en el primero, 582 en el segundo y 60 en el tercero.
En otras palabras, podemos precomputar para 1 <= k <= 5228758, "cantidad de numeros <= k no divisibles por un primo 4k+1, y
tal que el exponente de los 4k+3 es par".
-}
import Data.Array.Unboxed
import Elsantodel90.Primes

upperBound :: Integer
upperBound = 10^(11::Int)

maxMultiplier :: Int
maxMultiplier = fromInteger $ upperBound `div` (5*5*5 * 13*13)

goodMultiplier :: Int -> Bool
goodMultiplier = not . any badFactor . factorUsingArray (factorArray $ maxMultiplier)
                    where badFactor (p,_) = p `mod` 4 == 1

multiplierCount :: Array Int Integer
multiplierCount = array (0,maxMultiplier) ((0,0): arrayList )
                    where arrayList = [(i, multiplierCount ! (i-1) + term i) | i <- [1..maxMultiplier]]
                          term i | goodMultiplier i = toInteger i
                                 | otherwise        = 0

primes :: [Integer]
primes = map toInteger . filter (\x -> x `mod` 4 == 1) $ filter (primesUpToArray 10000000 !) [1..]

superList1 :: [Integer]
superList1 = [p *q*q * r*r*r | p <- takeWhile (<= pBound) primes, 
                               q <- takeWhile (\q -> q*q <= qBound p) primes,
                               p /= q,
                               r <- takeWhile (\r -> r*r*r <= rBound p q) primes,
                               r /= p, r /= q]
                 where pBound     = upperBound `div` (5*5*5 * 13*13)
                       qBound p   = upperBound `div` (p * 5*5*5)
                       rBound p q = upperBound `div` (p * q*q)

superList2 :: Integer -> Integer -> [Integer]
superList2 i j = [p^i * q^j | p <- takeWhile (\p -> p^i <= pBound) primes, 
                              q <- takeWhile (\q -> q^j <= qBound p) primes,
                              p /= q]
                 where pBound     = upperBound `div` (5^j)
                       qBound p   = upperBound `div` (p^i)

superList :: [Integer]
superList = superList1 ++ superList2 3 7 ++ superList2 2 10

countSolutions :: Integer -> Integer
countSolutions x = x * multiplierCount ! (fromInteger $ upperBound `div` x)


answer = sum $ map countSolutions superList

