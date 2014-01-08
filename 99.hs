import Elsantodel90.Parsing

main = readFile "99.in" >>= print . snd . maximum . map (\(i,[a,n]) -> (read n * log (read a), i)) . zip [1..] . map (split ',') . lines
