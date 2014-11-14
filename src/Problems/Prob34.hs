module Problems.Prob34 where
import Data.Array
import Elsantodel90.Digits
import Elsantodel90.Combinatorics hiding (factorial)
-- Suma digitos es como mucho 9!k. El numero es por lo menos 10^(k-1)
-- De ahi sale que no puede tener mas que 7 cifras, asi que 9999999 es una cota maxima.

facts :: Array Int Integer
facts = factorialsArray 9

factorial :: Integer -> Integer
factorial = (facts !) . fromInteger

sumFactDigits :: Integer -> Integer
sumFactDigits = sum . map factorial . rightToLeftDigits


answer = sum $ filter (\n -> n == sumFactDigits n) [3..9999999]
