import Data.List
import Elsantodel90.Primes

isPrime :: Integer -> Bool
isPrime = isPrimeWithPrec 10

main :: IO ()
main = print . maximum . filter isPrime . map (sum . zipWith (*) (iterate (*10) 1)) $ concatMap (\n -> permutations [1..n]) [1..9]
