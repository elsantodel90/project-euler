module Problems.Prob90 where

import Data.List
import Elsantodel90.IntPot
import Prelude hiding ((^))

extend :: [Integer] -> [Integer]
extend s | elem6 && elem9 = s
         | elem6          = 9:s
         | elem9          = 6:s
         | otherwise      = s
            where elem6 = 6 `elem` s
                  elem9 = 9 `elem` s
kSubsets :: Int -> [a] -> [[a]]
kSubsets k s | k > length s = []
             | k == 0       = [[]]
             | otherwise    = usingHead ++ notUsingHead
                where usingHead    = map (head s :) $ kSubsets (k-1) (tail s)
                      notUsingHead = kSubsets k (tail s)
                 
possibleCubes :: [[Integer]]
possibleCubes = kSubsets 6 [0..9]
squares :: [Integer]
squares = map (^2) [1..9]
allowsSquare :: Integer -> ([Integer], [Integer]) -> Bool
allowsSquare s (a,b) = good a b || good b a
                        where d1 = s `div` 10
                              d2 = s `mod` 10
                              good x y = elem d1 x && elem d2 y
allowsAllSquares :: ([Integer], [Integer]) -> Bool
allowsAllSquares p = all (\s -> allowsSquare s p) squares


answer :: Int
answer = length $ filter allowsAllSquares [(extend a,extend b) | a <- possibleCubes, b <- possibleCubes, a <= b]
