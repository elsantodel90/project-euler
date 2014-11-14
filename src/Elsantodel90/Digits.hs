module Elsantodel90.Digits(sumDigits, 
                           sumDigitsBase, 
                           rightToLeftDigits, 
                           leftToRightDigits, 
                           rightToLeftDigitsBase, 
                           leftToRightDigitsBase, 
                           digits) where

import Data.List

rightToLeftDigitsBase :: Integral a => a -> a -> [a]
rightToLeftDigitsBase k = map (`mod` k) . takeWhile (/=0) . iterate (`div` k)

rightToLeftDigits :: Integral a => a -> [a]
rightToLeftDigits = rightToLeftDigitsBase 10

leftToRightDigitsBase :: Integral a => a -> a -> [a]
leftToRightDigitsBase k = reverse . rightToLeftDigitsBase k

leftToRightDigits :: Integral a => a -> [a]
leftToRightDigits = leftToRightDigitsBase 10

sumDigitsBase :: Integral a => a -> a -> a
sumDigitsBase k = sum . rightToLeftDigitsBase k

sumDigits :: Integral a => a -> a
sumDigits = sumDigitsBase 10

digits :: (Integral a, Show a) => a -> a
digits = genericLength . show
