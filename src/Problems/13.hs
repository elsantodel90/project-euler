readInteger :: String -> Integer
readInteger = read

main :: IO ()
main = readFile "13.in" >>= putStrLn . take 10 . show . sum . map readInteger . lines
