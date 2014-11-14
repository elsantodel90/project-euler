module Problems.Prob75 where
import Data.List
import Elsantodel90.Pythagorean

topS :: Integer
topS = 1500000
topX :: Integer
topX = 1300 -- topX^2 > topS

s :: (Integer, Integer, Integer) -> Integer
s (a,b,c) = a+b+c


answer = length . filter (==1) . map length . 
                groupBy (\a b -> s a == s b) . 
                sortBy (\a b -> compare (s a) (s b)) . 
                concatMap (takeWhile ((<= topS) . s)) $ triplesWithMaxX topX
