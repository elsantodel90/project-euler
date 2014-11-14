module Problems.Prob58 where
import Data.List
import Elsantodel90.Primes
import Elsantodel90.IntPot
import Prelude hiding ((^))

maxPrime :: Integer
maxPrime = 100000

maxPrimeCheck :: Integer
maxPrimeCheck = maxPrime*maxPrime

primes :: [Integer]
primes = map toInteger $ primesUpTo (fromInteger maxPrime)

isPrime :: Integer -> Bool
isPrime n | n < 2 = False
          | n <= maxPrimeCheck = not . any (\p -> n `mod` p == 0) $ takeWhile (\x -> x^2 <=n) primes
          | otherwise          = error "Recompile with larger 'maxPrime'"

kAndPrimeCount :: [(Integer,Integer)]
kAndPrimeCount = tail $ iterate f (0,0)
                    where f (kPrev, cPrev) = (k, c)
                            where k = kPrev + 1
                                  c = cPrev + (genericLength $ filter isPrime [4*k^2-2*k+1,4*k^2+1, 4*k^2+2*k+1])

ratioUnder10 :: (Integer, Integer) -> Bool
ratioUnder10 (k,c) = 10 * c  < 1 * (1+4*k)


answer = (\(k,_) -> 2*k+1) . head $ filter ratioUnder10 kAndPrimeCount

