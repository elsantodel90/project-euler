module Problems.Prob35 where
import Data.Array.Unboxed
import Elsantodel90.Primes
import Elsantodel90.Digits

primeArray :: UArray Int Bool
primeArray = primesUpToArray 10000000

build :: [Int] -> Int
build = sum . zipWith (*) (iterate (*10) 1)

rotations :: [a] -> [[a]]
rotations l = take n . map (take n). iterate tail $ cycle l
                where n = length l

superPrime :: Int -> Bool
superPrime = all (primeArray !) . map build . rotations . rightToLeftDigits


answer = length $ filter superPrime [2..1000000]
