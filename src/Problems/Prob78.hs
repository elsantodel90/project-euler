module Problems.Prob78 where

import Data.Array
import Elsantodel90.Combinatorics

works :: Int -> Bool
works = (==0) . (partitionArrayModulus 1000000 60000 !)


answer = head $ filter works [0..]
