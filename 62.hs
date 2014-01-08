import qualified Data.Map as Map
import Data.List
import Data.Maybe

rep = sort . show
cubes :: [Integer]
cubes = map (^3) [1..]

f (m, best) x = (newMap , newBest)
                    where key = rep x
                          newVal = maybe [x] (x:) $ Map.lookup key m
                          newMap  = Map.insert key newVal m
                          newBest = max best (length newVal)

cubesMappings = scanl f (Map.empty,0) cubes
                            
target = 5
main = mapM_ print . map (minimum . snd) . Map.toList . Map.filter ((==target) . length) . fst . head . filter ((==target) . snd) $ cubesMappings
