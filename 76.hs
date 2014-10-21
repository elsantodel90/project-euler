import Data.Array
import Elsantodel90.Combinatorics

main :: IO ()
main = print $ (partitionArray 100 ! 100) - 1
