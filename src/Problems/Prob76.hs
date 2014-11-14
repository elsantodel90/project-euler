module Problems.Prob76 where

import Data.Array
import Elsantodel90.Combinatorics

answer = (partitionArray 100 ! 100) - 1
