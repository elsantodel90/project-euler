    
solve p = maximum $ foldl1 bestStep p
            where bestStep upper lower = head upper + head lower : dp ++ [last upper + last lower] -- length lower - length upper == 1
                                            where dp = zipWith (+) (tail lower) $ zipWith max upper (tail upper)

main = readFile "67.in" >>= print . solve . map (map read . words) . lines

