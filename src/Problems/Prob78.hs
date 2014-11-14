import Data.Array
import Elsantodel90.Combinatorics

works :: Int -> Bool
works = (==0) . (partitionArrayModulus 1000000 60000 !)

main :: IO ()
main = print . head $ filter works [0..]
