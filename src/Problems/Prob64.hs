module Problems.Prob64 where
import Elsantodel90.ContinuedFraction
import Elsantodel90.IntPot
import Prelude hiding ((^))

isSquare :: Integer -> Bool
isSquare n = (==n) . head . dropWhile (<n) $ map (^2) [1..]


answer = length . filter odd . map (cuadraticRealContinuedFractionPeriod 0 1) $ filter (not . isSquare) [1..10000]
