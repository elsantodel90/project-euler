module Problems.Prob47 where
import Elsantodel90.Primes

primeFactors :: Int -> [Int]
primeFactors = map fst . factorUsingArray (factorArray 1000000)

magicNumber :: Int -> Bool
magicNumber i = all (\x -> length x == magicCount) $ map primeFactors [i..i+magicCount - 1]
                where magicCount = 4

answer = head $ filter magicNumber [2..]

