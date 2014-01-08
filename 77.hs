import Data.Array.Unboxed
import Elsantodel90.Primes

maxN = 100
isPrime = (primesUpToArray maxN !)

dpArray :: Array (Int,Int) Int
dpArray = array ((0,0), (maxN, maxN)) $ recursiveCase ++ baseCase
            where baseCase = ((0,0),1) : [((n,0), 0) | n <- [1..maxN]]
                  recursiveCase = [((n, k), f n k) | n <- [1..maxN], k <- [1..n]]
                  f n k = (if isPrime k then dpArray ! (n-k, min k (n-k)) else 0) + dpArray ! (n,k-1)

f n = dpArray ! (n,n)

main = print . head $ filter ((>=5000) . f) [1..]