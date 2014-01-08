
upperBound :: Int -> Int
upperBound n = 2*k
                where  powersOfTwo = iterate (*2) 2
                       k = length $ takeWhile (<=n) powersOfTwo

bestChainFor :: Int -> Int
-- Notar que 2 en realidad no es candidato si justo n = 1. Pero igualmente funciona en ese caso.
bestChainFor n = backtrack n (upperBound n) 0 [1] [2]

merge :: Ord a => [a] -> [a] -> [a]
merge l1 []  = l1
merge [] l2  = l2
merge l1@(x:xs) l2@(y:ys) | x == y    = x : merge xs ys
                          | x < y     = x : merge xs l2
                          | otherwise = y : merge l1 ys

backtrack :: Int -> Int -> Int ->  [Int] -> [Int] -> Int
backtrack n bestSoFar products previous candidates 
    | null candidates = bestSoFar
    | candidate == n  = min (products+1) bestSoFar
    | products + 1 >= bestSoFar = bestSoFar
    | otherwise       = backtrack n intermediateBest (products+1) newPrevious newCandidates
         where (candidate:remainingCandidates) = candidates
               newPrevious   = candidate : previous
               newCandidates =  merge  remainingCandidates (reverse . filter (<= n) $ map (+candidate) newPrevious)
               withoutCandidate = backtrack n bestSoFar products previous remainingCandidates
               intermediateBest = min withoutCandidate bestSoFar

main :: IO ()
main = print . sum $ map bestChainFor [1..200]
