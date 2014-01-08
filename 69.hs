import Data.Ratio
import Elsantodel90.Primes

factArray = factorArray 1000000
phi = phiUsingArray factArray

main = print . snd $ maximum [(toInteger n % toInteger (phi n), n) | n <- [1..1000000]]
