f :: Integer -> Integer
f n | even n    = n `div` 2
    | otherwise = 3*n+1

collatzLength :: Integer -> Int
collatzLength = length . takeWhile (/= 1) . iterate f

main :: IO ()
main = print . snd . maximum $ map (\x -> (collatzLength x, x)) [1..10^(6 :: Int)-1]
