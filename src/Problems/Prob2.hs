module Problems.Prob2 where

fibo :: [Integer]
fibo = 1:2:zipWith (+) fibo (tail fibo)

answer :: Integer
answer = sum . filter even $ takeWhile (<= 4000000) fibo
