module Problems.Prob3 where
search :: Integer -> Integer -> Integer
search n k | k*k > n        = n
           | n `mod` k == 0 = search (n `div` k) k
           | otherwise      = search n (k+1)

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = search n 2

answer = largestPrimeFactor 600851475143
