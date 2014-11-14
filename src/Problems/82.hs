import Elsantodel90.Parsing
import Elsantodel90.Graph

solve :: [[Integer]] -> Integer
solve mat = gridDijkstra [(0,1),(1,0), (-1,0)] origins destinations mat
         where n = length mat
               m = length $ head mat
               origins      = [ (i,0  ) | i <- [0..n-1]]
               destinations = [ (i,m-1) | i <- [0..n-1]]

main :: IO ()
main = readFile "82.in" >>= print . solve . map (map read . split ',') . lines
