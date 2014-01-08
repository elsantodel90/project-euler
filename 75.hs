import Data.List
import Elsantodel90.Pythagorean

topS = 1500000
topX = 1300 -- topX^2 > topS

s (a,b,c) = a+b+c

main = print . length . filter (==1) . map length . 
                groupBy (\a b -> s a == s b) . 
                sortBy (\a b -> compare (s a) (s b)) . 
                concatMap (takeWhile ((<= topS) . s)) $ triplesWithMaxX topX
