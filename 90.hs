import Data.List

extend s | elem6 && elem9 = s
         | elem6          = 9:s
         | elem9          = 6:s
         | otherwise      = s
            where elem6 = 6 `elem` s
                  elem9 = 9 `elem` s

kSubsets k s | k > length s = []
             | k == 0       = [[]]
             | otherwise    = usingHead ++ notUsingHead
                where usingHead    = map (head s :) $ kSubsets (k-1) (tail s)
                      notUsingHead = kSubsets k (tail s)
                 
possibleCubes = kSubsets 6 [0..9]
squares = map (^2) [1..9]
allowsSquare s (a,b) = good a b || good b a
                        where d1 = s `div` 10
                              d2 = s `mod` 10
                              good a b = elem d1 a && elem d2 b
allowsAllSquares p = all (\s -> allowsSquare s p) squares

main = print . length $ filter allowsAllSquares [(extend a,extend b) | a <- possibleCubes, b <- possibleCubes, a <= b]
