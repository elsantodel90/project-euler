module Problems.Prob30 where
import Elsantodel90.Digits

sumFifth :: Integer -> Integer
sumFifth = sum . map (^(5 :: Int)) . rightToLeftDigits

good :: [Integer]
good = filter (\n -> n == sumFifth n) [1..1000000]


answer = sum $ tail good
