module Problems.Prob48 where
import Elsantodel90.IntPot
import Prelude hiding ((^))

answer = sum [i^(fromInteger i) | i <- [1..1000]] `mod` 10^10
