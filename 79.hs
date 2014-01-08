import Data.List

subsequence s [] = null s
subsequence [] l = True
subsequence s@(x:xs) (y:ys) | x == y    = subsequence xs ys
                            | otherwise = subsequence s ys

works input l = all (\s -> subsequence s l) input

-- Notar que tiene que tener al menos los digitos 0,1,2,3,6,7,8,9
-- Probando esos, vemos que hay uno solo en condiciones asi que listo.

candidates = permutations "01236789"

main = readFile "79.in" >>= mapM_ putStrLn . (\input -> filter (works input) candidates) . lines
