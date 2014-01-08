import Data.Char
import Data.List
import Elsantodel90.Parsing

scoreChar :: Char -> Int
scoreChar c = ord c - ord 'A' + 1

scoreWord :: String -> Int
scoreWord = sum . map scoreChar

main :: IO ()
main = readFile "22.in" >>= print . sum . zipWith (*) [1..] . map scoreWord . sort . wordListLineRead
