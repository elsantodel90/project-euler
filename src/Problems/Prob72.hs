module Problems.Prob72 where
import Elsantodel90.Primes

top :: Int
top = 1000000
phi :: Int -> Int
phi = phiUsingArray $ factorArray top


answer = sum $ map (toInteger . phi) [2..top]
