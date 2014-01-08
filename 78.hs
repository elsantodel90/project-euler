import Data.Array
import Elsantodel90.Combinatorics

works = (==0) . (partitionArrayModulus 1000000 60000 !)

main = print . head $ filter works [0..]
