import Data.Array.IArray
import Elsantodel90.Primes
import Elsantodel90.IntPot
import Prelude hiding ((^))

doubledSquares :: [Integer]
doubledSquares = map ((*2) . (^2)) [0..]

primes :: [Integer]
primes = map toInteger $ filter (primesUpToArray 10000 !) [2..]

summable :: [Integer] -> [Integer] -> Integer -> Bool
summable _ [] _ = False
summable a@(x:xs) b@(y:ys) n | x+y == n  = True
                             | x+y > n   = summable a ys n
                             | otherwise = summable xs b n
summable _ _ _ = error "BUG! ESTO NUNCA DEBERIA PASAR!!"
                             
works :: Integer -> Bool
works n = summable primes (reverse $ takeWhile (<= n) doubledSquares) n

main :: IO ()
main = print $ filter (not . works) [1,3..] !! 1
