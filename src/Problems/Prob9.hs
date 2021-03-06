module Problems.Prob9 where

sqr :: Integer -> Integer
sqr x = x*x

answer = x*y*z
         where (x,y,z) = head ([(a,b,c) | a <- [1..1000], b <- [a+1..1000], c <- [b+1..1000], sqr a + sqr b == sqr c, a+b+c == 1000] :: [(Integer,Integer,Integer)])
