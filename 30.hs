import Elsantodel90.Digits

sumFifth :: Integer -> Integer
sumFifth = sum . map (^(5 :: Int)) . rightToLeftDigits

good :: [Integer]
good = filter (\n -> n == sumFifth n) [1..1000000]

main :: IO ()
main = print . sum $ tail good
