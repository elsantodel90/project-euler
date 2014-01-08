import Elsantodel90.ContinuedFraction

isSquare n = (==n) . head . dropWhile (<n) $ map (^2) [1..]

main = print . length . filter odd . map (cuadraticRealContinuedFractionPeriod 0 1) $ filter (not . isSquare) [1..10000]
