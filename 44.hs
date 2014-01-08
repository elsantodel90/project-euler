import Data.Maybe

-- Usando el main2 para buscar, encontramos rapidamente (1560090,7042750), con diferencia 5482660. 
-- Luego tenemos cota para la diferencia maxima.
-- Al correr el main, que prueba completamente todas las opciones relativo a esa cota, obtenemos la misma cota,
-- o sea que en realidad era optima. O sea que con menos codigo podiamos tirarnos el lance y pegarle.

maxDiff :: Integer
maxDiff = 5482660

pentagonal :: Integer -> Integer
pentagonal n = (n * (3*n-1)) `div` 2

pentagonals :: [Integer]
pentagonals = map pentagonal [1..]

renice :: Integer -> Integer -> Bool
renice a b = all isPentagonal [a+b, abs (a-b)]

{-
nice :: Integer -> Integer -> Bool
nice i j = renice a b
            where a = pentagonal i
                  b = pentagonal j

main2 :: IO ()
main2 = mapM_ print $ [(i,j, pentagonal i, pentagonal j) | s <- [1..10000], i <- [1..(s-1)`div`2], let j = s - i, nice i j]
-}

integerSqrt :: Integer -> Maybe Integer
integerSqrt n | k*k == n  = Just k
              | otherwise = Nothing
                    where k = round . (sqrt :: Double -> Double) $ fromInteger n

isPentagonal :: Integer -> Bool
isPentagonal x = case integerSqrt (1 + 24 * x) of
                    Just k  -> k`mod`3 == 2
                    Nothing -> False

getDifferences :: ([Integer],[Integer]) -> [Integer]
getDifferences (a,b) = zipWith diff b $ takeWhile (< head b) a
                        where diff x y | renice x y = x - y
                                       | otherwise  = 0
                        
main :: IO ()
main = print . maximum . map maximum . map getDifferences . takeWhile distinctHeads $ iterate advance (pentagonals, tail pentagonals)
        where advance (a,b) = (dropWhile (\q -> (b !! 1) - q > maxDiff) a, tail b)
              distinctHeads (l1,l2) = head l1 /= head l2
