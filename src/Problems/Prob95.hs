module Problems.Prob95 where

import Data.Set
import Data.List hiding (insert)
import Elsantodel90.Primes
import Elsantodel90.IntPot
import Prelude hiding ((^))

maxElement :: Int
maxElement = 10^6

divisors :: Int -> [Int]
divisors = divisorsUsingArray $ factorArray 1000000

f :: Int -> Int
f n = if next > maxElement then 0 else next
        where next = sum (divisors n) - n

addAll :: Ord b => Set b -> Set b -> Set b
addAll l s = Data.Set.foldl' (flip insert) s l

valAux :: Set Int -> Int -> Set Int -> ((Int, Int), Set Int)
valAux processing n seen
            | n `member` processing = (if elem 1 fcycle then (0,0) else (length fcycle, -minimum fcycle) , newSeen)
            | n `member` seen = ((0,0),newSeen) 
            | n > maxElement  = ((0,0),newSeen)
            | otherwise       = valAux (insert n processing) (f n) seen
                    where fcycle   = n : (takeWhile (/= n) . tail $ iterate f n)
                          newSeen = addAll processing seen

val :: Int -> Set Int -> ((Int, Int), Set Int)
val = valAux empty


answer = negate . snd . fst $ Data.List.foldl' combine ((0 , 0), empty) [1..maxElement]
            where combine (bestPair, seen) n = seq newBest (newBest, newSeen)
                        where (p, newSeen) = val n seen
                              newBest      = max bestPair p
