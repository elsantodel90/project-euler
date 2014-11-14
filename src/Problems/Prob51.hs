module Problems.Prob51 where
import Data.Array.Unboxed
import Elsantodel90.Primes

maxPrime :: Int
maxPrime = 1000000
primesArray :: UArray Int Bool
primesArray = primesUpToArray maxPrime
primes :: [Int]
primes = filter (primesArray !) [2..]

plug :: Int -> Int -> Int
plug d x = x*10+d

replacements :: Int -> Int -> [Int]
replacements _ 0 = [0]
replacements k p = map (plug u) rest ++ map (plug k) rest
                    where u = p`mod`10
                          r = p`div`10
                          rest = replacements k r
                          
nonTrivialReplacements :: Int -> Int -> [Int]
nonTrivialReplacements k p = tail $ replacements k p

familyLength :: Int
familyLength = 8 -- Con 6 da 13, con 7 da 56003

familyList :: Int -> [[Int]]
familyList p = filter ((>= familyLength) . length) . filter (elem p) . map (filter ((==d) . length . show) . filter (primesArray !). map head) . takeWhile (not . null . head) . iterate (map tail) . map ($p) $ map nonTrivialReplacements [0..9]
                 where d = length $ show p
                 
magicPrime :: Int -> Bool
magicPrime = not . null . familyList


answer = fst . (\x -> (x, familyList x)) . head $ filter magicPrime primes
