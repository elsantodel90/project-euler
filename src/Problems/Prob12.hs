module Problems.Prob12 where
import Elsantodel90.Primes

factor :: Integer -> [(Integer,Int)]
factor = factorUsingPrimeList . map toInteger $ primesUpTo 10000
                  
divisors :: Integer -> Integer
divisors = product . map ((+1) . toInteger . snd) . factor


answer = head $ filter ((>500). divisors) [(n * (n+1)) `div` 2 | n <- [1..]]

