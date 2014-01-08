import Data.Char

groups :: Int -> [a] -> [[a]]
groups k = map (take k) . takeWhile (not . null) . iterate tail

main :: IO ()
main = readFile "8.in" >>= print . maximum . map product . groups 5 . map (\c -> ord c - ord '0') . concat . lines
