import Elsantodel90.Parsing
import Elsantodel90.Graph

solve :: [[Integer]] -> Integer
solve mat = gridDijkstra [(0,1),(1,0), (-1,0), (0,-1)] [(0,0)] [(n-1,m-1)] mat
         where n = length mat
               m = length $ head mat

main :: IO ()
main = readFile "83.in" >>= print . solve . map (map read . split ',') . lines
