import Data.List
import Data.Ratio
import Elsantodel90.Primes

maxNum = 10^7
factArray = factorArray maxNum
phi = phiUsingArray factArray

permu a b = sort (show a) == sort (show b)

main = print . snd $ minimum [(toInteger n % toInteger f, n) | n <- [2..maxNum], let f = phi n, permu n f]
