module Problems.Prob99 where

import Elsantodel90.Parsing

answer_calculator :: IO Integer
answer_calculator = readFile "99.in" >>= return . snd . maximum . map (\(i,[a,n]) -> (read n * log (read a :: Double), i)) . zip [(1 :: Integer)..] . map (split ',') . lines
