module Problems.Prob13 where
readInteger :: String -> Integer
readInteger = read

answer_calculator :: IO Integer
answer_calculator = readFile "13.in" >>= return . read . take 10 . show . sum . map readInteger . lines
