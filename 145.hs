mix :: [a] -> [[a]] -> [[a]]
mix elements l = [x : xs | x <- elements, xs <- l]

numbers :: Int -> [[Integer]]
numbers n = foldr mix [[]] $ [1..9] : replicate (n-1) [0..9]

reversiblesWithNDigitsGivenPrefix :: Int -> [Integer] -> Integer
reversiblesWithNDigitsGivenPrefix n prefix
    | even n         = product $ corrected [5 - x `div` 2 | x <- prefix] -- Notar la correccion para excluir el caso de terminar en 0, que solo puede pasar con los pares
    | n `mod` 4 == 3 = 5 * product [if odd i then x `div` 2 else 5 - (x+1) `div` 2 | (i, x) <- zip [1 :: Int ..] prefix]
    | otherwise      = 0 -- Este caso se podia haber separado antes para optimizar si eso valiera la pena.
                where corrected (x:xs) | even x    = x:xs
                                       | otherwise = (x-1):xs
                      corrected _ = error "La lista de digitos no deberia ser vacia jamas"

reversiblesWithNDigits :: Int -> Integer
reversiblesWithNDigits n = 
        sum [reversiblesWithNDigitsGivenPrefix n prefix | prefix <- numbers (n `div` 2)]

main :: IO ()
main = print . sum $ map reversiblesWithNDigits [1..9]
