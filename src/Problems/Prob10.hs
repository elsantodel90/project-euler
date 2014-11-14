module Problems.Prob10 where

import Elsantodel90.Primes

-- Pasamos a Integer para hacer la suma para evitar overflows

answer = sum . map toInteger $ primesUpTo 2000000
