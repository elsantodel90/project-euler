divSum :: Integer -> Integer
divSum n = sum $ filter ((==0) . mod n) [1..n-1]

amicable :: Integer -> Bool
amicable n = b /= n && divSum b == n
                where b = divSum n

main :: IO ()
main = print . sum $ filter amicable [1..10000]
