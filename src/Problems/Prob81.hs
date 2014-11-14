module Problems.Prob81 where

import Elsantodel90.Parsing
import Elsantodel90.Graph

solve :: [[Integer]] -> Integer
solve mat = gridDijkstra [(0,1),(1,0)] [(0,0)] [(n-1,m-1)] mat
         where n = length mat
               m = length $ head mat

answer_calculator :: IO Integer
answer_calculator = readFile "inputs/81.in" >>= return . solve . map (map read . split ',') . lines
