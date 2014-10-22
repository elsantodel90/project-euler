import Elsantodel90.Parsing

main :: IO ()
main = readFile "99.in" >>= print . snd . maximum . map (\(i,[a,n]) -> (read n * log (read a :: Double), i)) . zip [(1 :: Integer)..] . map (split ',') . lines
