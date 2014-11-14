module Problems.Prob4 where
palindrome :: String -> Bool
palindrome l = l == reverse l

palindromic :: Integer -> Bool
palindromic =  palindrome . show

tresDigitos :: [Integer]
tresDigitos = [100..999]

answer = maximum [n | a <- tresDigitos, b <- tresDigitos, let n = a*b, palindromic n]
