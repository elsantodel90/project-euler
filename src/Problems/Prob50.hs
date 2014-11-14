module Problems.Prob50 where
import Data.List
import Data.Maybe
import Data.Array.Unboxed
import Elsantodel90.Primes

maxPrime :: Int
maxPrime = 1000000

isValidPrime :: Int -> Bool
isValidPrime n = n <= maxPrime && primesUpToArray maxPrime ! n

primes :: [Int]
primes = filter isValidPrime [2..maxPrime]

upperBound :: Int
upperBound = length . tail . takeWhile (<= maxPrime) $ scanl (+) 0 primes

possibleLength :: Int -> Maybe Int
possibleLength l = case filter isValidPrime . map sum . filter ((==l) . length) . map (take l) $ tails primes of
                      [] -> Nothing
                      li -> Just $ head li


answer = head . catMaybes $ map possibleLength [upperBound, upperBound-1..1]
