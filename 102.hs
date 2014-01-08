import Elsantodel90.Parsing
import Elsantodel90.Geometry hiding ((*))

import Prelude hiding ((^))

parse l = [Vector2D a b, Vector2D c d, Vector2D e f]
          where [a,b,c,d,e,f] = map read $ split ',' l

sameSign a b c = a * b > 0 && b * c > 0

containsZero [p1,p2,p3] = sameSign (p1 ^ p2) (p2 ^ p3) (p3 ^ p1)

main = readFile "102.in" >>= print . length . filter containsZero . map parse . lines