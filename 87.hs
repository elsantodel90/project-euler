import Data.List
import Elsantodel90.Primes
import Elsantodel90.Numeric

maxN :: Int
maxN = 50000000
primes = primesUpTo . fromInteger $ integerSqrt 1 (toInteger maxN)

rawNumberList = [a^2 + b^3 + c^4 | a <- primes, 
                                   b <- takeWhile (\b -> a^2 + b^3 < maxN) primes, 
                                   c <- takeWhile (\c -> a^2 + b^3 + c^4 < maxN) primes]

sortUniq = map head . group . sort
numberList = sortUniq rawNumberList

main = print $ length numberList
