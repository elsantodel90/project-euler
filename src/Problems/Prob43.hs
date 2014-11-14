module Problems.Prob43 where
import Data.List

primeList :: [Integer]
primeList = reverse [2,3,5,7,11,13,17]

divides :: Integer -> Integer -> Bool
a `divides` b = b `mod` a == 0

num :: [Integer] -> Integer
num = sum . zipWith (*) (iterate (*10) 1)

isMagical :: [Integer] -> Bool
isMagical perm = and $ zipWith divides primeList terms
                    where terms       = [num . take 3 $ drop k perm | k <- [0..]]
                          

answer = sum . map num . filter isMagical $ permutations [0..9]
