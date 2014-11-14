module Problems.Prob63 where

-- Queremos contar los los (a,n) con a,n enteros positivos tales que
-- 10^(n-1) <= a^n < 10^n
-- La segunda equivale a que a este en [1..9]
-- Si 10^(n-1) > 9^n, ese n no tiene chances. Pero
-- Esto pasa a partir de n = 22 (induccion + verificar el caso). Luego n es a lo mas 21.

works :: Integer -> Integer -> Bool
works a n =  10^(n-1) <= a^n -- a in [1..9]

answer = length [(a^n,a,n) | a <- [1..9], n <- [1..21], works a n]
