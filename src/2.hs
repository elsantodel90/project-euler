fibo :: [Integer]
fibo = 1:2:zipWith (+) fibo (tail fibo)

main :: IO ()
main = print $ sum . filter even $ takeWhile (<= 4000000) fibo
