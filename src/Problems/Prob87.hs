module Problems.Prob87 where

import Data.List
import Elsantodel90.Primes
import Elsantodel90.Numeric
import Elsantodel90.IntPot
import Prelude hiding ((^))

maxN :: Int
maxN = 50000000
primes :: [Int]
primes = primesUpTo . fromInteger $ integerSqrt 1 (toInteger maxN)

rawNumberList :: [Int]
rawNumberList = [a^2 + b^3 + c^4 | a <- primes, 
                                   b <- takeWhile (\b -> a^2 + b^3 < maxN) primes, 
                                   c <- takeWhile (\c -> a^2 + b^3 + c^4 < maxN) primes]

sortUniq :: [Int]->[Int]
sortUniq = map head . group . sort
numberList :: [Int]
numberList = sortUniq rawNumberList

answer = length numberList
