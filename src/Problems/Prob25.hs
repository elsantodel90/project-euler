fibo :: [Integer]
fibo = 1:1:zipWith (+) fibo (tail fibo)

fiboIndexed :: [(Integer,Integer)]
fiboIndexed = zip [1..] fibo

digitCount :: Integer -> Int
digitCount = length . show

main :: IO ()
main = print . fst . head $ dropWhile ( (< 1000) . digitCount . snd) fiboIndexed
