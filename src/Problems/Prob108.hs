module Problems.Prob108 where

import Elsantodel90.Primes
import Data.Array.Unboxed
import Data.List

primes :: [Integer]
primes = map toInteger $ filter (primesUpToArray 100 !) [2..]

-- Upper bound for the number of primes that can appear in the solution
primesBound :: Integer -> Integer
primesBound n = genericLength $ takeWhile (<n) powersOfThree
                  where powersOfThree = iterate (*3) 1

candidates :: Integer -> Integer -> [Integer] -> Integer -> Integer -> [Integer]
candidates maxExponent n p divCount acc
    | null p    = if divCount > n then [acc] else []
    | otherwise = concat [candidates i n (tail p) (divCount * pow) (acc * prime^i) | (i,pow) <- zip [0..] candidatePowers]
                    where prime = head p
                          candidatePowers = takeWhile (\x -> (x - 2) * divCount <= n) $ genericTake (maxExponent + 1) [1,3..]

firstOverN :: Integer -> Integer
firstOverN n = minimum $ candidates n (2 * n - 1) (genericTake k primes) 1 1
                where k = primesBound n

answer = firstOverN 1000
