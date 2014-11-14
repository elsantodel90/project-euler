module Problems.Prob5 where
answer = foldr lcm 1 [1..20 :: Integer]
