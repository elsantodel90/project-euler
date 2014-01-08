module Elsantodel90.BinarySearch(binarySearch) where

-- a cumple, b no cumple, a < b
binarySearch :: (Integer -> Bool) -> Integer -> Integer -> (Integer, Integer)
binarySearch p a b | b - a == 1 = (a,b)
                   | p c        = binarySearch p c b 
                   | otherwise  = binarySearch p a c
                                where c = (a+b) `div` 2
