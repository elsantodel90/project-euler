module Elsantodel90.Polynomial(
        Polynomial,
        xPol,
        zeroPol,
        onePol,
        leadingCoefficient,
        independentTerm,
        constantPolynomial,
        interpolating,
        eval,
        degree
        ) where

import Data.List

newtype Polynomial a = Polynomial {coefficients :: [a]} deriving Eq

varSymbol :: String
varSymbol = "X"

coefficientsShow :: (Eq a, Num a, Show a) => [a] -> String
coefficientsShow [] = "0"
coefficientsShow l  = concat . intersperse " + " . filter (not . null) . map monomialShow $ zip [(0 :: Integer)..] l
                            where monomialShow (_,0) = ""
                                  monomialShow (0,a) = show a
                                  monomialShow (1,a) = condShow a ++ varSymbol
                                  monomialShow (k,a) = condShow a ++ varSymbol ++ "^" ++ show k
                                  condShow 1    = ""
                                  condShow (-1) = "-"
                                  condShow x    = show x

instance (Eq a, Num a ,Show a) => Show (Polynomial a) where
    show = coefficientsShow . coefficients

zeroPol :: Num a => Polynomial a
zeroPol = Polynomial []

onePol :: Num a => Polynomial a
onePol = Polynomial [1]

xPol :: Num a => Polynomial a
xPol = Polynomial [0,1]

lift :: ([a]->[b]) -> Polynomial a -> Polynomial b
lift f = Polynomial . f . coefficients

lift2 :: ([a]->[b]->[c]) -> Polynomial a -> Polynomial b -> Polynomial c
lift2 f p1 p2 = Polynomial $ f (coefficients p1) (coefficients p2)

normalize :: (Eq a, Num a) => Polynomial a -> Polynomial a
normalize = lift $ reverse . dropWhile (==0) . reverse

coefficientsPlus :: (Eq a, Num a) => [a] -> [a] -> [a]
coefficientsPlus a b = take l $ zipWith (+) (a ++ zeros) (b ++ zeros)
                        where l     = max (length a) (length b)
                              zeros = repeat 0

coefficientsProduct :: (Eq a, Num a) => [a] -> [a] -> [a]
coefficientsProduct a = foldl' coefficientsPlus [] . zipWith (++) (inits $ repeat 0) . map (\k -> map (*k) a)

leadingCoefficient :: Polynomial a -> a
leadingCoefficient = last . coefficients

independentTerm :: Polynomial a -> a
independentTerm = head . coefficients

constantPolynomial :: (Eq a, Num a) => a -> Polynomial a
constantPolynomial 0 = zeroPol
constantPolynomial x = Polynomial [x]

coefficientsEval :: Num a => [a] -> a -> a
coefficientsEval p x = foldr (\a acum -> a + acum * x) 0 p

eval :: Num a => Polynomial a -> a -> a
eval = coefficientsEval . coefficients

degree :: Polynomial a -> Int
degree = subtract 1 . length . coefficients

instance (Eq a ,Num a) => Num (Polynomial a) where
    p1 + p2 = normalize $ lift2 coefficientsPlus p1 p2
    (*) = lift2 coefficientsProduct
    negate = lift $ map negate
    abs p = p * signum p
    signum = constantPolynomial . signum . leadingCoefficient
    fromInteger = constantPolynomial . fromInteger

-- Inneficient O(N^3) lagrange interpolation. N^2 is achieved using Ruffini's Rule
interpolating :: (Eq a ,Fractional a) => [(a,a)] -> Polynomial a
interpolating l = sum $ map p [0..length l-1]
                where p i = constantPolynomial yi * product [(xPol - constantPolynomial xj) * (constantPolynomial . recip $ xi - xj ) | (xj,_) <- l, xi /= xj]
                        where (xi, yi) = l !! i
