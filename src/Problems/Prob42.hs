module Problems.Prob42 where
import Data.Char
import Elsantodel90.Parsing

triangleNumbers :: [Integer]
triangleNumbers = [(n*(n+1)) `div` 2 | n <- [1..]]

isTriangleNumber :: Integer -> Bool
isTriangleNumber n = elem n $ takeWhile (<=n) triangleNumbers

charScore :: Char -> Integer
charScore c = toInteger $ ord c - ord 'A' + 1

wordScore :: String -> Integer
wordScore = sum . map charScore

isTriangleWord :: String -> Bool
isTriangleWord = isTriangleNumber . wordScore

answer_calculator :: IO Int
answer_calculator = readFile "42.in" >>= return . length . filter isTriangleWord . wordListLineRead
