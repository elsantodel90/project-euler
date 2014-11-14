module Problems.Prob86 where

import Elsantodel90.Pythagorean
import Elsantodel90.Numeric

countSolutions :: Integer -> Integer -> Integer -> Integer
countSolutions intA a bPlusC | a /= intA     = 0
                             | bPlusC > 2*a  = 0
                             | bPlusC >= a+1 = a - (bPlusC+1) `div` 2 + 1
                             | bPlusC >= 2   = bPlusC `div` 2
                             | otherwise     = 0

-- Para calcular f a, alcanza con mirar las ternas pitagoricas de la pinta a^2 + (b+c)^2 = z^2 con a >= b >= c >= 1
-- De ahi sale que el z^2 es como mucho 5a^2 y el z en cuestion como mucho raiz[5] a
-- La cantidad de tales ternas es f a
-- Las ternas primitivas son de la pinta (x^2 - y^2, 2*x*y, x^2 + y^2)
-- De aca sale que este x es tal que x^2 <= raiz[5] a <= 3a. Esto nos da una cota para saber hasta donde mirar de ternas primitivas
f :: Integer -> Integer
f a = sum [countSolutions a x y + countSolutions a y x | l <- triplesWithMaxX xBound, (x,y,_) <- takeWhile notTooBig l]
        where xBound            = integerSqrt a (3 * a)
              notTooBig (x,y,_) = min x y <= a


answer = fst . head . dropWhile ((<1000000) . snd) $ iterate step (0,0)
        where step (i,s) = (i+1, f (i+1) + s)
