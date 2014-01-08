import Data.Array.Unboxed
import Elsantodel90.Primes
import Elsantodel90.IntPot
import Prelude hiding ((^))

primeArray :: UArray Int Bool
primeArray = primesUpToArray 100000

poly :: Integer -> Integer -> [Integer]
poly a b = [n^2 + a * n + b | n <- [0..]]

isPrime :: Integer -> Bool
isPrime n = n >= 2 && primeArray ! (fromInteger n)

value :: Integer -> Integer -> Int
value a b = length . takeWhile isPrime $ poly a b

main = print . snd $ maximum [(value a b, a*b) | a <- interval, b <- interval]
        where interval = [-999..999]
