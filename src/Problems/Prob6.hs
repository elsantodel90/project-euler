module Problems.Prob6 where

sqr :: Integer -> Integer
sqr x = x*x

answer = sqr (sum [1..100]) - sum (map sqr [1..100])
