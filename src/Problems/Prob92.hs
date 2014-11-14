module Problems.Prob92 where

import Data.Array
import Elsantodel90.Digits
import Elsantodel90.IntPot
import Prelude hiding ((^))

f :: Int -> Int
f  = sum . map (^2) . rightToLeftDigits

directLoopClass :: Int -> Int
directLoopClass = head . dropWhile (not . (`elem` [1,89])) . iterate f

maxStored :: Int
maxStored = 1000
loopClassArray :: Array Int Int
loopClassArray = array (1,maxStored) [(i, directLoopClass i) | i <- [1..maxStored]]

loopClass :: Int -> Int
loopClass n | n <= maxStored = loopClassArray ! n
            | otherwise      = loopClass (f n)


answer = length $ filter ((==89) . loopClass) [1..10000000]
