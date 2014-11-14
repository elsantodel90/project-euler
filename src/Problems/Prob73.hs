module Problems.Prob73 where
import Data.Ratio


answer = length . filter (<1%2) $ filter (>1%3) [n%d | d <- [1..12000 :: Integer], n <- [d `div` 3..(d+1)`div`2], gcd n d == 1]
