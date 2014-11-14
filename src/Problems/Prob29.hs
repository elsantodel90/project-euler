module Problems.Prob29 where
import Data.List

sortUnique :: Ord a => [a] -> [a]
sortUnique = map head . group . sort


answer = length $ sortUnique [a^b | a <- [2..100 :: Integer], b <- [2..100 :: Integer]]
