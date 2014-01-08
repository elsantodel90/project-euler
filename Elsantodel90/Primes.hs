module Elsantodel90.Primes(primesUpTo , 
                           primesUpToArray, 
                           factorArray ,
                           factorUsingArray, 
                           factorUsingPrimeList,
                           divisorsUsingArray,
                           divisorsUsingPrimeList,
                           phiUsingArray, 
                           phiUsingPrimeList,
                           isPrimeWithPrec) where

import System.Random
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import Elsantodel90.ModularArithmetic

sqr :: Num a => a -> a
sqr x = x*x

-- Erathostenes Sieve
primesUpToArray :: Int -> UArray Int Bool
primesUpToArray n = runSTUArray $
               do
                arr <- newArray (0,n) True :: ST s (STUArray s Int Bool)
                writeArray arr 0 False
                writeArray arr 1 False
                let work i = readArray arr i >>= (\isP -> if isP
                                then forM_ [i*i, i*i+i..n] (\k -> writeArray arr k False) 
                                else return () )
                mapM_ work $ takeWhile ((<= n) . sqr) [2..n]
                return arr

factorArray :: Int -> UArray Int Int
factorArray n = runSTUArray $
               do
                arr <- newArray (0,n) 1 :: ST s (STUArray s Int Int)
                let work i = readArray arr i >>= (\f -> if f == 1
                                then forM_ [i*i, i*i+i..n] (\k -> writeArray arr k i) 
                                else return () )
                mapM_ work $ takeWhile ((<= n) . sqr) [2..n]
                return arr

factorUsingArray :: UArray Int Int -> Int -> [(Int, Int)]
factorUsingArray a n | n == 1    = []
                     | f == 1    = [(n,1)]
                     | otherwise = (f,e):factorUsingArray a reduced
                        where f = a ! n
                              (divided,reduced:_) = span ((==0) . (`mod` f)) $ iterate (`div` f) n
                              e = length divided

factorUsingPrimeList :: [Integer] -> Integer -> [(Integer, Int)]
factorUsingPrimeList (p:ps) n | n == 1    = []
                              | p*p > n   = [(n,1)]
                              | e > 0     = (p,e):factorUsingPrimeList ps reduced
                              | otherwise = factorUsingPrimeList ps n
                                    where (divided,reduced:_) = span ((==0) . (`mod` p)) $ iterate (`div` p) n
                                          e = length divided
factorUsingPrimeList _ _ = error "Prime list is not long enough!!!!"

divisorsFromFactorization :: Integral a => [(a,Int)] -> [a]
divisorsFromFactorization [] = [1]
divisorsFromFactorization ((p,alpha):r) = [a * b | a <- take (alpha+1) (iterate (*p) 1), b <- divisorsFromFactorization r]

divisorsUsingArray :: UArray Int Int -> Int -> [Int]
divisorsUsingArray a = divisorsFromFactorization . factorUsingArray a

divisorsUsingPrimeList :: [Integer] -> Integer -> [Integer]
divisorsUsingPrimeList a = divisorsFromFactorization . factorUsingPrimeList a

phiFactor :: Integral a => (a, Int) -> a
phiFactor (p, e) = p^(e-1) * (p-1)

phiFromFactorization :: Integral a => [(a,Int)] -> a
phiFromFactorization = product . map phiFactor

phiUsingArray :: UArray Int Int -> Int -> Int
phiUsingArray arr = phiFromFactorization  . factorUsingArray arr

phiUsingPrimeList :: [Integer] -> Integer -> Integer
phiUsingPrimeList l = phiFromFactorization  . factorUsingPrimeList l

primesUpTo :: Int -> [Int]
primesUpTo n = filter (primesUpToArray n !) [2..n]

randomSequence :: Integer -> [Integer]
randomSequence n = randomRs (1,n-1) $ mkStdGen 414523

sieveSize :: Integer
sieveSize = 150000000

smallerPrimes :: UArray Int Bool
smallerPrimes = primesUpToArray $ fromInteger sieveSize

-- Rabin - Miller
isPrimeWithPrec :: Int -> Integer -> Bool
isPrimeWithPrec prec n | n <= sieveSize = smallerPrimes ! fromInteger n
                       | otherwise      = not . or . map (rabinMillerWitness n) . take prec $ randomSequence n

rabinMillerWitness :: Integer -> Integer -> Bool
rabinMillerWitness n a = ad /= 1 && not (elem (n-1) . take s $ iterate (modsqr n) ad)
                            where ad = modexp n a d
                                  s = length evens
                                  (evens, d:_) = span even div2
                                  div2 = iterate (`div` 2) (n-1)

