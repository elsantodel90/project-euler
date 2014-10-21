import Data.List

polygonal :: Integer -> [Integer]
polygonal k = dropWhile (<1000) $ takeWhile (<10000) [(n * ((k-2) * n + 4 - k)) `div` 2 | n <- [1..]]

sequencesOrderings :: [[[Integer]]]
sequencesOrderings = map (polygonal 8:) $ permutations (map polygonal [3..7])

chainable :: Integer -> Integer -> Bool
chainable a b = a `mod` 100 == b `div` 100

findSolution :: [[Integer]] -> [[Integer]]
findSolution (s:rest) = concatMap (\x -> findSolutionAcum x x [x] rest ) s
findSolution _        = error "Lista vacia aqui no deberia ocurrir"

findSolutionAcum :: Integer -> Integer -> [Integer] -> [[Integer]] -> [[Integer]]
findSolutionAcum first prev acum [] = if chainable prev first then [acum] else []
findSolutionAcum first prev acum (s:rest) =  concat [findSolutionAcum first x (x:acum) rest | x <- s, chainable prev x]

main :: IO ()
main = mapM_ print . map sum $ concatMap findSolution sequencesOrderings
