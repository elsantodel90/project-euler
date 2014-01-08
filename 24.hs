
splits :: [a] -> [(a,[a])]
splits l = [(x, a++b) | i <- [0..length l-1], let (a, x:b) = splitAt i l]

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations l = concat [map (x:) (permutations xs) | (x,xs) <- splits l]

main :: IO ()
main = putStrLn $ permutations ['0'..'9'] !! 999999
