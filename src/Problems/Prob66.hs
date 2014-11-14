module Problems.Prob66 where
import Data.Ratio
import Elsantodel90.ContinuedFraction
import Elsantodel90.IntPot
import Prelude hiding ((^))

isSquare :: Integer -> Bool
isSquare n = (==n) . head . dropWhile (<n) $ map (^2) [1..]

solvePell :: Integer -> (Integer, Integer)
solvePell n = head . dropWhile (not . solution) . map (\r -> (numerator r, denominator r)) . convergents $ cuadraticRealContinuedFraction 0 1 n
                where solution (x,y) = x^2 - n * y^2 == 1


answer = snd . maximum . map (\d -> (solvePell d, d)) $ filter (not . isSquare) [1..1000]

