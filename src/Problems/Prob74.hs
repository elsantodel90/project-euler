module Problems.Prob74 where
import Data.Array
import Elsantodel90.Digits
import Elsantodel90.Combinatorics

f :: Int -> Integer
f = sum . map (factorialsArray 10 !) . rightToLeftDigits

len :: Integer -> Integer
-- Length 1 loops calculated using main2
len 1     = 1
len 2     = 1
len 145   = 1
len 40585 = 1
-- Loops given in statement
len 169 = 3
len 363601 = 3
len 1454 = 3

len 871 = 2
len 45361 = 2

len 872 = 2
len 45362 = 2

len x = 1 + len (f $ fromInteger x)

_main2 :: IO ()
_main2 = mapM_ print $ filter (\x -> f x == toInteger x) [1..1000000]


answer = length $ filter ((==60) . len) [1..1000000-1]
