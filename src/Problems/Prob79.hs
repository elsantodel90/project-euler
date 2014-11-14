module Problems.Prob79 where

import Data.List

subsequence :: String -> String -> Bool
subsequence s [] = null s
subsequence [] _ = True
subsequence s@(x:xs) (y:ys) | x == y    = subsequence xs ys
                            | otherwise = subsequence s ys

works :: [String] -> String -> Bool
works input l = all (\s -> subsequence s l) input

-- Notar que tiene que tener al menos los digitos 0,1,2,3,6,7,8,9
-- Probando esos, vemos que hay uno solo en condiciones asi que listo.

candidates :: [String]
candidates = permutations "01236789"

answer_calculator :: IO Integer
answer_calculator = readFile "79.in" >>= return . read . head . (\input -> filter (works input) candidates) . lines
