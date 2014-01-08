import Elsantodel90.IntPot
import Prelude hiding ((^))

main :: IO ()
main = print $ sum [i^i | i <- [1..1000]] `mod` 10^10
