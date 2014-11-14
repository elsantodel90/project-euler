
triangles :: [Integer]
triangles   = [(n * (n+1)) `div` 2 | n <- [1..]]

pentagonals :: [Integer]
pentagonals = [(n * (3*n-1)) `div` 2 | n <- [1..]]

hexagonals :: [Integer]
hexagonals  = [n * (2*n-1) | n <- [1..]]

intersection :: [Integer] -> [Integer] -> [Integer]
intersection _ [] = []
intersection [] _ = []
intersection a@(x:xs) b@(y:ys) | x < y     = intersection xs b
                               | x > y     = intersection a ys
                               | otherwise = x : intersection xs ys

superNumbers :: [Integer]
superNumbers = foldr1 intersection [triangles,pentagonals,hexagonals]

main :: IO ()
main = print $ superNumbers !! 2
