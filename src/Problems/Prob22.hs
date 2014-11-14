module Problems.Prob22 where
import Data.Char
import Data.List
import Elsantodel90.Parsing

scoreChar :: Char -> Int
scoreChar c = ord c - ord 'A' + 1

scoreWord :: String -> Int
scoreWord = sum . map scoreChar

answer_calculator :: IO Int
answer_calculator = readFile "22.in" >>= return . sum . zipWith (*) [1..] . map scoreWord . sort . wordListLineRead
