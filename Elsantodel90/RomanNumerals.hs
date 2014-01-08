module Elsantodel90.RomanNumerals(fromNumeral, toNumeral, normalizedNumeral) where

-- Module assumes upper case

charVal 'I' = 1
charVal 'V' = 5
charVal 'X' = 10
charVal 'L' = 50
charVal 'C' = 100
charVal 'D' = 500
charVal 'M' = 1000

-- Asumes numeral is valid, that is: Numerals must be arranged in descending order of size (considering substractive pairs).
fromNumeral :: String -> Integer
fromNumeral []  = 0
fromNumeral [c] = charVal c
fromNumeral (a:b:rest) | valA < valB = valB - valA + fromNumeral rest
                       | otherwise   = valA + fromNumeral (b:rest)
                            where valA = charVal a
                                  valB = charVal b

-- Standard form
toNumeral :: Integer -> String
toNumeral n = thousands ++ hundreds ++ tens ++ units
                where nThousands = fromInteger $  n `div` 1000
                      nHundreds  = fromInteger $ (n `mod` 1000) `div` 100
                      nTens      = fromInteger $ (n `mod` 100) `div` 10
                      nUnits     = fromInteger $ n `mod` 10
                      thousands  = replicate nThousands 'M'
                      hundreds   = digitPattern 'C' 'D' 'M'  !! nHundreds
                      tens       = digitPattern 'X' 'L' 'C'  !! nTens
                      units      = digitPattern 'I' 'V' 'X'  !! nUnits
                      -- unit, five, ten
                      digitPattern u f t = [[],[u],[u,u],[u,u,u],[u,f],[f],[f,u],[f,u,u],[f,u,u,u],[u,t]]

normalizedNumeral :: String -> String
normalizedNumeral = toNumeral . fromNumeral
