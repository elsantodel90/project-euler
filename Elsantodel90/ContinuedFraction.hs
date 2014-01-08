-- TODO: Hacer mas eficiente el calculo de convergents, en este momento calcula cada convergent desde 0

module Elsantodel90.ContinuedFraction(ContinuedFraction,
                                      convergent,
                                      convergents,
                                      eContinuedFraction,
                                      cuadraticRealContinuedFraction,
                                      cuadraticRealContinuedFractionPeriod) where

import Elsantodel90.BinarySearch
import Data.Ratio
import Data.List

type ContinuedFraction = [Integer]

convergent :: Fractional a => Integer -> ContinuedFraction -> a
convergent i (x:xs) | i == 0    = fromInteger x
                    | otherwise = fromInteger x + 1 / convergent (i-1) xs

convergents :: Fractional a => ContinuedFraction -> [a]
convergents c = [convergent i c | i <- [0..]]

eContinuedFraction :: ContinuedFraction
eContinuedFraction = 2 : concatMap (\k -> [1,2*k,1]) [1..]

-- floor a b n = el mayor entero x tal que 
-- x <= a + b sqrt n
-- x-a <= b sqrt n [implica x >= a]
-- (x-a)^2 <= b^2 n
cuadraticRealFloor :: Rational -> Rational -> Integer -> Integer
cuadraticRealFloor a b n = fst $ binarySearch (\x -> (fromInteger x-a)^2 <= b^2 * fromInteger n) 0 (numerator $ abs a + abs b * fromInteger n)

type FullContinuedFraction = [(Integer, Rational, Rational)]

-- cuadraticRealContinuedFraction a b n gives the continued fraction for a + b (sqrt n)

cuadraticRealFullContinuedFraction :: Rational -> Rational -> Integer -> FullContinuedFraction
cuadraticRealFullContinuedFraction a b n = (k,a,b) : cuadraticRealFullContinuedFraction newA newB n
                                             where k    = cuadraticRealFloor a b n 
                                                   den  = -(a-fromInteger k)^2 + b^2 * fromInteger n
                                                   newA = (fromInteger k-a) / den
                                                   newB = b / den 

cuadraticRealContinuedFraction :: Rational -> Rational -> Integer -> ContinuedFraction
cuadraticRealContinuedFraction a b = map (\(k,_,_) -> k) . cuadraticRealFullContinuedFraction a b

periodWithPrev :: FullContinuedFraction ->  FullContinuedFraction  -> Int
periodWithPrev prev (x:xs) = case elemIndex x prev of
                                  Nothing -> periodWithPrev (x:prev) xs
                                  Just n  -> n + 1

cuadraticRealContinuedFractionPeriod :: Rational -> Rational -> Integer -> Int
cuadraticRealContinuedFractionPeriod a b = periodWithPrev [] . cuadraticRealFullContinuedFraction a b
