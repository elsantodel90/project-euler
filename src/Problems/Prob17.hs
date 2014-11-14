module Problems.Prob17 where
numToString :: Integer -> String
numToString 1 = "one"
numToString 2 = "two"
numToString 3 = "three"
numToString 4 = "four"
numToString 5 = "five"
numToString 6 = "six"
numToString 7 = "seven"
numToString 8 = "eight"
numToString 9 = "nine"

numToString 10 = "ten"
numToString 11 = "eleven"
numToString 12 = "twelve"
numToString 13 = "thirteen"
numToString 14 = "fourteen"
numToString 15 = "fifteen"
numToString 16 = "sixteen"
numToString 17 = "seventeen"
numToString 18 = "eighteen"
numToString 19 = "nineteen"

numToString 20 = "twenty"
numToString 30 = "thirty"
numToString 40 = "forty"
numToString 50 = "fifty"
numToString 60 = "sixty"
numToString 70 = "seventy"
numToString 80 = "eighty"
numToString 90 = "ninety"

numToString 1000 = "onethousand"

numToString n | n < 100 = concatMap numToString [tens, unit]
              | rest == 0 = numToString hundredsDigit ++ "hundred"
              | otherwise = numToString hundredsDigit ++ "hundredand" ++ numToString rest
                    where unit = n`mod` 10
                          tens = rest - unit
                          rest = n `mod` 100
                          hundredsDigit = n`div` 100

--main = mapM_ (print . numToString) [1..1000]


answer = length $ concatMap numToString [1..1000]
