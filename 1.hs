answer :: Integer
answer = sum $ filter interesting [1..999]
            where interesting n = n `mod` 3 == 0 || n `mod` 5 == 0

main :: IO ()
main = print answer
