module Problems.Prob28 where
import Elsantodel90.IntPot
import Prelude hiding ((^))

-- Rect i tiene [ (2i-1)^2 + 1  .. (2i+1)^2] (i > 0)

spiral :: Integer -> Integer -> Integer
spiral x y | d == 0  = 1
           | x == d && y == -d  = (2*d+1)^2
           | x == d  = (2*d-1)^2 + y + d
           | x == -d = (2*d-1)^2 + 4*d + d - y
           | y == d  = (2*d-1)^2 + 2*d + d - x
           | y == -d = (2*d-1)^2 + 6*d + x + d
           | otherwise = error "Ups! Case missing! Shame on you!"
                where d = max (abs x) (abs y)

answer = sum [spiral x x + spiral x (-x) | x <- [-500..500]] - 1
