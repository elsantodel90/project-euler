import Data.Array
import Data.Ratio

-- Hay que calcular la probabilidad p de que gane el jugador, y la respuesta es floor (1/p) - 1

steps = 15

beta True  = 1
beta False = 0

dpArray :: Array (Int,Int) Rational
dpArray = array ((1, -steps), (steps+1, steps)) (baseCases ++ recursiveCases)
            where baseCases = [((steps+1, k), beta (k > 0)) | k <- [-steps..steps]]
                  recursiveCases = [((ni, k) , (dpArray ! (ni+1, k+1) + n * dpArray ! (ni+1, k-1)) / (n+1)) | ni <- [1..steps], k <- [-(ni-1)..(ni-1)], let n = toRational ni]
            

rationalFloor r = numerator r `div` denominator r

main = print $ rationalFloor (1 / (dpArray ! (1,0)))
