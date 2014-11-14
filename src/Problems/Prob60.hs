module Problems.Prob60 where
import Data.Array.Unboxed
import Elsantodel90.Primes

isPrime :: Integer -> Bool
isPrime = isPrimeWithPrec 5

-- Interesting primes (those that could theoretically be part of the solution tuple)
maxToTest :: Int
maxToTest = 27000

interestingPrimes :: [Int]
interestingPrimes = primesUpTo maxToTest

-- For each prime, lower ones that work
firstPot :: Int -> Int
firstPot b = head . dropWhile (<=b) $ iterate (*10) 1

firstPotArray :: UArray Int Int
firstPotArray = array (1,maxToTest) [(i, firstPot i) | i <- [1..maxToTest]]

concatNumbers :: Int -> Int -> Integer
concatNumbers a b = toInteger a * toInteger (firstPotArray ! b) + toInteger b

works2 :: Int -> Int -> Bool
works2 a b = isPrime (concatNumbers a b) && isPrime (concatNumbers b a)

friends :: Array Int [Int]
friends = array (2,maxToTest) $ map (\p -> (p,filter (works2 p) (takeWhile (<p) interestingPrimes))) interestingPrimes

areFriends :: Int -> Int -> Bool
areFriends a b = b `elem` friends ! a

answer = minimum  [a+b+c+d+e | a <- interestingPrimes,
                                     let friendsOfA = friends ! a,
                                     b <- friendsOfA,
                                     c <- takeWhile (<b) friendsOfA, areFriends b c,
                                     d <- takeWhile (<c) friendsOfA, areFriends b d, areFriends c d,
                                     e <- takeWhile (<d) friendsOfA, areFriends b e, areFriends c e, areFriends d e
                        ]

