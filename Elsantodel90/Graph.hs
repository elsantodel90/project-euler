module Elsantodel90.Graph(bfs, dijkstra, gridDijkstra) where

import Prelude hiding (lookup)
import Data.Sequence as Seq hiding (length, adjust, filter, zip)
import Data.Set as Set hiding (map, filter, foldr, member)
import Data.Map as Map hiding (map, filter, foldr)
import Data.Maybe

bfs :: Ord a => [a] -> (a -> [a]) -> Map a Int
bfs initialNodes neighbors = getResultFromIteration $ iterate step (initialSeq, initialMap)
    where getResultFromIteration = snd . head . dropWhile (not . Seq.null . fst)
          initialSeq = Seq.fromList initialNodes
          initialMap = Map.fromList (zip initialNodes (repeat 0))
          step (startingQueue, distanceMap) = (newQueue, newMap)
            where x :< queue   = viewl startingQueue
                  d            = fromJust (lookup x distanceMap) + 1
                  newNeighbors = filter (not . (`member` distanceMap )) $ neighbors x
                  newQueue     = foldr (flip (|>)) queue newNeighbors
                  newMap       = foldr (\key dict -> Map.insert key d dict) distanceMap newNeighbors

dijkstra :: Ord a => [a] -> (a -> [(a,Integer)]) -> Map a Integer
dijkstra initialNodes neighbors = getResultFromIteration $ iterate step (initialQueue, initialMap)
    where getResultFromIteration = snd . head . dropWhile (not . Set.null . fst)
          initialQueue = Set.fromList (zip (repeat 0) initialNodes)
          initialMap   = Map.fromList (zip initialNodes (repeat 0))
          step (startingQueue, distanceMap) = foldr processNeighbor (queue, distanceMap) neighborhood 
            where ((d,z) , queue)    = Set.deleteFindMin startingQueue
                  neighborhood       = neighbors z
                  processNeighbor (x,c) (q, dm) | betterCostFound = (Set.insert (nc, x) deletedSet, Map.insert x nc dm)
                                                | otherwise       = (q , dm)
                            where oldcMaybe = lookup x dm
                                  oldc      = fromJust oldcMaybe
                                  nc        = d + c
                                  betterCostFound = isNothing oldcMaybe || nc < oldc
                                  deletedSet      = if isNothing oldcMaybe then q else Set.delete (oldc, x) q

gridDijkstra :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> [[Integer]] -> Integer
gridDijkstra movements origins destinations costMatrix = minimum $ map nodeCost destinations
                    where distanceMap = dijkstra [(-1,-1)] f
                          nodeCost p  = fromJust $ lookup p distanceMap
                          f (-1,-1)   = map costPair origins
                          f p         = map costPair $ neighbors p
                          costPair p@(i,j) = (p, costMatrix !! i !! j)
                          n = length costMatrix
                          m = length $ head costMatrix
                          inRange (i,j) = 0 <= i && i < n && 0 <= j && j < m
                          neighbors (i,j) = filter inRange [(i+x, j+y) | (x,y) <- movements]
                
