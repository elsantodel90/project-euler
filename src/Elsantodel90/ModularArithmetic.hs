module Elsantodel90.ModularArithmetic(modsum, modprod, modsqr, modexp) where

modsum :: Integer -> Integer -> Integer -> Integer
modsum m a b = (a+b) `mod` m

modprod :: Integer -> Integer -> Integer -> Integer
modprod m a b = (a*b) `mod` m

modsqr :: Integer -> Integer -> Integer
modsqr m x = (x*x) `mod` m

modexp :: Integer -> Integer -> Integer -> Integer
modexp m a b | b == 0    = 1
             | odd b     = modprod m a base
             | otherwise = base
                where base = modexp m (modsqr m a) (b `div` 2)
