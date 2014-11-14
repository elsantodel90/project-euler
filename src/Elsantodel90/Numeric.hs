module Elsantodel90.Numeric(integerSqrt, innerProduct, solveLinearSystem) where

import Elsantodel90.IntPot
import Prelude hiding ((^))

isIntegerSqrt :: Integer -> Integer -> Bool
isIntegerSqrt x r = x^2 <= r && (x+1)^2 > r

-- Calculates floor (sqrt r) for r > 0, given an initial estimate x, 1 <= x <= r.
integerSqrt :: Integer -> Integer -> Integer
integerSqrt x r  | isIntegerSqrt x r = x
                 | otherwise         = integerSqrt ((x + r `div` x) `div` 2) r

-- Linear system resolution by gaussian elimination + backward substitution.

innerProduct :: Num a => [a] -> [a] -> a
innerProduct a b = sum $ zipWith (*) a b

lambdaAplusB :: Fractional a => a -> [a] -> [a] -> [a]
lambdaAplusB lambda = zipWith (\x y -> lambda * x + y)

ensureNonZero :: (Fractional a, Ord a) => [[a]] -> [[a]]
ensureNonZero matrix = if j == 0  then matrix 
                                  else (row : tail previous) ++ (head previous : following)
                               where j = snd $ maximum [(abs (head kthRow), k) | (k, kthRow) <- zip [0..] matrix]
                                     (previous,row:following) = splitAt j matrix

gaussianElimination :: (Fractional a, Ord a) => [[a]] -> [[a]]
gaussianElimination [] = []
gaussianElimination matrix = row : gaussianElimination (map zeroRow (tail nonZero))
                        where nonZero = ensureNonZero matrix
                              row     = head nonZero
                              d       = head row
                              zeroRow r = tail $ lambdaAplusB (- (head r) / d ) row r

backwardSubstitution :: (Fractional a, Ord a) => [[a]] -> [a]
backwardSubstitution = foldr step []
                        where step extendedRow partialX = x : partialX
                                where partialXCoeficients = tail $ init extendedRow
                                      xCoeficient = head extendedRow
                                      y = last extendedRow
                                      x = (y - innerProduct partialXCoeficients partialX) / xCoeficient
                                              

-- Assumes matrix is an invertible square matrix (extendedMatrix includes "y" values in Ax = y)
solveLinearSystem :: (Fractional a, Ord a) => [[a]] -> [a]
solveLinearSystem = backwardSubstitution . gaussianElimination
