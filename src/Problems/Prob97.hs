module Problems.Prob97 where

import Elsantodel90.IntPot
import Prelude hiding ((^))

answer = ((28433*2^7830457+1) `mod` (10^10) :: Integer)
