module Problems.Prob23 where
import Elsantodel90.Primes
import Data.Array

upperBound :: Integer
upperBound=28123

abundantRange :: [Integer]
abundantRange = [1..upperBound]

divisors :: Integer -> [Integer]
divisors = divisorsUsingPrimeList . map toInteger $ primesUpTo 1000

divSum :: Integer -> Integer
divSum = sum . divisors

abundant :: Integer -> Bool
abundant n = divSum n > 2*n

abundants :: [Integer]
abundants = filter abundant abundantRange

abundantArray :: Array Integer Bool
abundantArray = accumArray (\_ newVal -> newVal) True (1,upperBound) . flip zip (repeat False) $ filter (<= upperBound) [a+b | a <- abundants, b <- abundants]


answer = sum  $ filter (abundantArray !) abundantRange
