module Problems.Prob103 where

import Data.Set (Set, singleton, union, intersection, empty, findMax, findMin)
import qualified Data.Set as Set
import Data.Maybe

setString :: [Integer] -> String
setString = concatMap show

combineOldWithNew :: Integer -> Set Integer -> Set Integer -> Maybe (Set Integer)
combineOldWithNew number oldSetToAdd oldSetToMerge
        | not . Set.null $ intersection newSet oldSetToMerge = Nothing -- Dos conjuntos de igual suma
        | otherwise = Just $ union newSet oldSetToMerge
                where newSet = Set.map (+number) oldSetToAdd

validSets :: [Set Integer] -> Bool
validSets sets = and $ zipWith (\s1 s2 -> findMax s1 < findMin s2) sets (tail sets)

backtracking :: Integer -> [Integer] -> Integer -> (Integer, [Integer]) -> Integer -> [Set Integer] -> (Integer, [Integer])
backtracking currentSum currentNumbers remainingNumbers bestPair nextInteger sets
        | remainingNumbers == 0 = (currentSum, currentNumbers)
        | canPrune              = bestPair
        | otherwise             = backtracking currentSum currentNumbers remainingNumbers bestUsingNumber (nextInteger + 1) sets
                where bestInteger   = fst bestPair
                      canPrune      = sumLowerBound >= bestInteger
                      sumLowerBound = currentSum +  ((2 * nextInteger + remainingNumbers - 1) * remainingNumbers) `div` 2 
                      maybeSetsUsingNumber = Just (singleton 0) : zipWith (combineOldWithNew nextInteger) sets (tail sets ++ [empty])
                      bestUsingNumber | any isNothing maybeSetsUsingNumber = bestPair
                                      | invalidSets setsUsingNumber        = bestPair
                                      | otherwise = backtracking (currentSum + nextInteger) (nextInteger:currentNumbers) (remainingNumbers - 1) bestPair (nextInteger + 1) setsUsingNumber
                      setsUsingNumber = catMaybes maybeSetsUsingNumber
                      invalidSets = not . validSets
                      
optimumSpecialSumSetWithUpperbound :: Integer -> Integer -> [Integer]
optimumSpecialSumSetWithUpperbound n upperBound = reverse . snd $ backtracking 0 [] n (upperBound, []) 1 [singleton 0]

answer :: Integer
answer = read . setString $ optimumSpecialSumSetWithUpperbound 7 256
