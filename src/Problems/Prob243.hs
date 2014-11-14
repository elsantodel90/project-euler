module Problems.Prob243 where

import Elsantodel90.Primes

-- Se puede demostrar haciendo cuentitas que dado un numero n, si consideramos cualquier numero que resulta
-- de cambiar en su factorizacion en primos un primo p por otro q < p tal que q no aparezca
-- en la factorizacion de n, entonces la resilience no crece.
-- Esto demuestra inmediatamente que el d buscado tiene como primos en su factorizacion exactamente
-- a los primeros k primos, para cierto k.

-- Probando primoriales, encontramos rapidamente que este es el primero que funciona
primes :: [Integer]
primes = [2,3,5,7,11,13,17,19,23,29]
upperBound :: Integer
upperBound = product primes

generateCandidates :: [Integer] -> Integer -> [Integer]
generateCandidates [] acum = [acum]
generateCandidates (p:ps) acum = concatMap (generateCandidates ps) acums
                            where powers = iterate (*p) 1
                                  acums  = takeWhile (<= upperBound) $ map (*acum) powers

candidates :: [Integer]
candidates = generateCandidates primes 1

-- We search for minimum d with resilience < a/b
a,b :: Integer
a=15499
b=94744

phi :: Integer -> Integer
phi = toInteger . phiUsingPrimeList (map toInteger $ primesUpTo 10000000) . fromInteger


answer = minimum $ filter (\n -> phi n * b < a * (n-1) ) candidates
