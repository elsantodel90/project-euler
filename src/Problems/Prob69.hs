module Problems.Prob69 where
import Data.Ratio
import Elsantodel90.Primes
import Data.Array.Unboxed

factArray :: UArray Int Int
factArray = factorArray 1000000

phi :: Int -> Int
phi = phiUsingArray factArray


answer = snd $ maximum [(toInteger n % toInteger (phi n), n) | n <- [1..1000000]]
