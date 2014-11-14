module Problems.Prob67 where
solve :: [[Integer]] -> Integer    
solve p = maximum $ foldl1 bestStep p
            where bestStep upper lower = head upper + head lower : dp ++ [last upper + last lower] -- length lower - length upper == 1
                                            where dp = zipWith (+) (tail lower) $ zipWith max upper (tail upper)

answer_calculator :: IO Integer
answer_calculator = readFile "inputs/67.in" >>= return . solve . map (map read . words) . lines

