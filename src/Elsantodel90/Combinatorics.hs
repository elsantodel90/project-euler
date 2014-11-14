module Elsantodel90.Combinatorics(choose, 
                                  factorial, 
                                  factorialsArray, 
                                  factorials, 
                                  partitionArrayModulus, 
                                  partitionArray) where

import Data.Array.ST
import Control.Monad.ST
import Data.Array

-- n choose k, factorials
    
choose :: Integer -> Integer -> Integer
n `choose` k = product [n-k+1..n] `div` product [1..k]

factorial :: Integer -> Integer
factorial n = product [1..n]

factorialsArray :: Int -> Array Int Integer
factorialsArray n = listArray (0,n) $ take (n+1) factorials

factorials :: [Integer]
factorials = 1:zipWith (*) [1..] factorials

-- Partitions

generalizedPentagonals :: [Int]
generalizedPentagonals = concat [ [p n, p (-n)] | n <- [1..]]
                            where p n = (n * (3*n-1)) `div` 2

type Operation = Integer -> Integer -> Integer -> Integer

strangeSum :: Operation -> [Integer] -> Integer
strangeSum _ []  = 0
strangeSum _ [a] = a
strangeSum op (a:b:xs) = op a b (strangeSum op xs)

modularOp :: Integer -> Integer -> Integer -> Integer -> Integer
modularOp modulus a b c = (a+b-c) `mod` modulus

directOp :: Integer -> Integer -> Integer -> Integer
directOp a b c = a+b-c

partitionArrayOp :: Operation -> Int -> Array Int Integer
partitionArrayOp op n = runSTArray $
                        do
                          arr <- newArray_ (0,n) :: ST s (STArray s Int Integer)
                          writeArray arr 0 1
                          let work i = calc i >>= writeArray arr i >> forceEval i
                              calc i = fmap (strangeSum op) $ mapM (readArray arr) indexes
                                        where indexes = [i - k | k <- takeWhile (<=i) generalizedPentagonals]
                              forceEval i = readArray arr i >>= (\x -> seq x (return ()))
                          mapM_ work [1..n]
                          return arr

partitionArrayModulus :: Integer -> Int -> Array Int Integer
partitionArrayModulus modulus = partitionArrayOp $ modularOp modulus

partitionArray :: Int -> Array Int Integer
partitionArray = partitionArrayOp directOp
