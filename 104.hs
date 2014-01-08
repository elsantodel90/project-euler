import Elsantodel90.IntPot
import Prelude hiding ((^))

hasAll :: Eq a => [a] -> [a] -> Bool
hasAll elems l = all (`elem` l) elems

firstPandigital :: Double -> Bool
firstPandigital = hasAll ['1'..'9'] . take 10 . show -- Tomamos 10 por el . decimal :)

modularPandigital :: Integer -> Bool
modularPandigital = hasAll ['1'..'9'] . show

fibDoubleStep :: (Double, Double) -> (Double, Double)
fibDoubleStep (a,b) = if c >= 10.0 then (b / 10.0, c / 10.0) else (b,c)
                        where c = a+b

fibModularStep :: (Integer, Integer) -> (Integer, Integer)
fibModularStep (a,b) = (b, (a+b) `mod` 10^9)

coolFib :: Integer -> (Integer,Integer) -> (Double, Double) -> Integer
coolFib i mods@(_,b) doubles@(_,bb) = if modularPandigital b && firstPandigital bb 
                                          then i 
                                          else coolFib (i+1) (fibModularStep mods) (fibDoubleStep doubles)

main :: IO ()
main = print $ coolFib 1 (0,1) (0.0, 1.0)
