module Problems.Prob62 where
import Elsantodel90.IntPot
import Prelude hiding ((^))
import qualified Data.Map as Map
import Data.List

rep :: Integer -> String
rep = sort . show

cubes :: [Integer]
cubes = map (^3) [1..]

f :: (Map.Map String [Integer], Int) -> Integer -> (Map.Map String [Integer], Int)
f (m, best) x = (newMap , newBest)
                    where key = rep x
                          newVal = maybe [x] (x:) $ Map.lookup key m
                          newMap  = Map.insert key newVal m
                          newBest = max best (length newVal)

cubesMappings :: [(Map.Map String [Integer], Int)]
cubesMappings = scanl f (Map.empty,0) cubes

target :: Int                            
target = 5

answer :: Integer
answer = head . map (minimum . snd) . Map.toList . Map.filter ((==target) . length) . fst . head . filter ((==target) . snd) $ cubesMappings
