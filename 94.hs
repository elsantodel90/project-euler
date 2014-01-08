import Elsantodel90.Numeric

isSquare n = (integerSqrt 1 n)^2 == n

maxPerim = 10^9

-- It can be proved that 4 | X - 1, and solutions must be of the following form.

oddCount = f 1 1 + f 2 (-1)
              where f m d = sum $ takeWhile (<= maxPerim) [3*x+d | k <- [1..], let x = m * k^2+d, x >= 2, x `mod` 4 == 1, isSquare ((3*x+d) `div` m)]

main = print $ oddCount
