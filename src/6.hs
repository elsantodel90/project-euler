
sqr :: Integer -> Integer
sqr x = x*x

main :: IO ()
main = print $ sqr (sum [1..100]) - sum (map sqr [1..100])
