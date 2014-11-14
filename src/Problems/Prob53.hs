module Problems.Prob53 where
import Elsantodel90.Combinatorics


answer = length $ filter ((>1000000) . uncurry choose)  [(n,k) | n <- [1..100], k <- [0..n]]
