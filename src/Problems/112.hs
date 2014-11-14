import Elsantodel90.Digits

bouncy :: Integer -> Bool
bouncy = bounces . adjacentComparisons . rightToLeftDigits
          where adjacentComparisons l = zipWith compare l (tail l)
                bounces comparisons   = LT `elem` comparisons &&
                                        GT `elem` comparisons
beta :: Bool -> Integer
beta True = 1
beta False = 0

ratios :: [(Integer, Integer)]
ratios = tail . scanl f (0,0) $ map bouncy [1..]
           where f (k,n) isBouncy = (k + beta isBouncy, n+1)

main :: IO ()
main = print . snd . head $ filter (\(k,n) -> 100*k == 99*n) ratios
