import Data.List
import Data.Ratio
import Data.Array.Unboxed
import Elsantodel90.Primes
import Elsantodel90.IntPot
import Prelude hiding ((^))

maxNum :: Int
maxNum = 10^7
factArray :: UArray Int Int
factArray = factorArray maxNum
phi :: Int -> Int
phi = phiUsingArray factArray

permu :: (Show a, Show b) => a -> b -> Bool
permu a b = sort (show a) == sort (show b)

main :: IO ()
main = print . snd $ minimum [(toInteger n % toInteger f, n) | n <- [2..maxNum], let f = phi n, permu n f]
