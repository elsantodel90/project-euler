module Problems.Prob11 where

deltas :: [(Int,Int)]
deltas = [(1,1),(1,0),(0,1),(1,-1)]

products :: [[Integer]] -> [Integer]
products mat = concatMap diagAux deltas
            where n = length mat
                  m = length $ head mat
                  get i j = (mat !! i) !! j
                  inRange (i,j) = 0 <= i && i < n && 0 <= j && j < m
                  diagAux (dx,dy) = [a*b*c*d | ax <- [0..n-1], ay <- [0..n-1], inRange (ax+3*dx, ay+3*dy) ,
                                                      let a = get ax ay, 
                                                      let b = get (ax+dx) (ay+dy),
                                                      let c = get (ax+2*dx) (ay+2*dy),
                                                      let d = get (ax+3*dx) (ay+3*dy)]

answer_calculator :: IO Integer
answer_calculator = readFile "11.in" >>= return . maximum . products . map (map read . words) . lines
