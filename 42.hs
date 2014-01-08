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

main :: IO ()
main = readFile "42.in" >>= print . length . filter isTriangleWord . wordListLineRead
