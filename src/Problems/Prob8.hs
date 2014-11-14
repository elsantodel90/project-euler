module Problems.Prob8 where

import Data.Char

groups :: Int -> [a] -> [[a]]
groups k = map (take k) . takeWhile (not . null) . iterate tail

answer_calculator :: IO Int
answer_calculator = readFile "8.in" >>= return . maximum . map product . groups 5 . map (\c -> ord c - ord '0') . concat . lines
