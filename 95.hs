import Data.Set
import Data.List hiding (insert)
import Elsantodel90.Primes

maxElement = 10^6

divisors = divisorsUsingArray $ factorArray 1000000

f n = if next > maxElement then 0 else next
        where next = sum (divisors n) - n

addAll l s = Data.Set.foldl' (flip insert) s l

valAux processing n seen
            | n `member` processing = (if elem 1 cycle then (0,0) else (length cycle, -minimum cycle) , newSeen)
            | n `member` seen = ((0,0),newSeen) 
            | n > maxElement  = ((0,0),newSeen)
            | otherwise       = valAux (insert n processing) (f n) seen
                    where cycle   = n : (takeWhile (/= n) . tail $ iterate f n)
                          newSeen = addAll processing seen

val = valAux empty

main = print . negate . snd . fst $ Data.List.foldl' combine ((0 , 0), empty) [1..maxElement]
            where combine (bestPair, seen) n = seq newBest (newBest, newSeen)
                        where (p, newSeen) = val n seen
                              newBest      = max bestPair p
