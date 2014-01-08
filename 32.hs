import Data.List

build :: [Integer] -> Integer
build = sum . zipWith (*) (iterate (*10) 1)

magicProducts :: [Integer]
magicProducts = [f3 | l <- permutations [1..9], a <- [1..7], b <- [1..8-a], 
                        let f1 = build (take a l), 
                        let f2 = build (take b $ drop a l),
                        let f3 = build (drop a $ drop b l),
                        f1 * f2 == f3]

main :: IO ()
main = print . sum $ nub magicProducts
