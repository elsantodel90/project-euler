import Elsantodel90.IntPot
import Prelude hiding ((^))

main :: IO ()
main = print $ ((28433*2^7830457+1) `mod` (10^10) :: Integer)
