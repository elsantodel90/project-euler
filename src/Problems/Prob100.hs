module Problems.Prob100 where

import Data.Maybe
import Elsantodel90.Numeric
import Elsantodel90.IntPot
import Prelude hiding ((^))

-- Despejando notamos que debe ser (2B-1)^2 = T^2 + (T-1)^2
-- En otras palabras, T-1 , T, 2B-1 ha de ser terna pitagorica primitiva.
-- De la formulita x^2 - y^2, 2xy, x^2 + y^2 sale x = y + sqrt (2y^2 +- 1)
-- Lo cual nos dice que x es aprox (1+raiz 2)y
-- De ahi que 2xy es aprox 2(1+raiz 2)y^2
-- Como queremos que sea al menos 10^12, 2(1+raiz 2)y^2 = 10^12 => y es aprox 455090

isSquare :: Integer -> Bool
isSquare n = (integerSqrt 1 n)^2 == n

tryY :: Integer -> Maybe (Integer, Integer)
tryY y | isSquare root1 = Just $ ((x1^2 + y^2 + 1) `div` 2, x1^2 - y^2)
       | isSquare root2 = Just $ ((x2^2 + y^2 + 1) `div` 2, 2 * x2 * y)
       | otherwise      = Nothing
           where root1 = 2 * y^2 + 1
                 root2 = 2 * y^2 - 1
                 x1     = y + integerSqrt 1 root1
                 x2     = y + integerSqrt 1 root2


answer = fst . head . catMaybes  $ map tryY [450000..]



