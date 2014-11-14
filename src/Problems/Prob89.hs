module Problems.Prob89 where

import Elsantodel90.RomanNumerals

savedCharacters :: String -> Int
savedCharacters numeral = length numeral - length (normalizedNumeral numeral)

answer_calculator :: IO Int
answer_calculator = readFile "89.in" >>= return . sum . map savedCharacters  . lines
