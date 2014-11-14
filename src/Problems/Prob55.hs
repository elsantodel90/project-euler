module Problems.Prob55 where
f :: Integer -> Integer
f x = x + (read $ (reverse $ show x))

palindromic :: Integer -> Bool
palindromic x = show x == reverse (show x)

lychrel :: Integer -> Bool
lychrel = (== 50) . length . takeWhile (not . palindromic) . take 50 . tail . iterate f


answer = length $ filter lychrel [1..9999]
