import Elsantodel90.Combinatorics

main :: IO ()
main = print . length $ filter ((>1000000) . uncurry choose)  [(n,k) | n <- [1..100], k <- [0..n]]
