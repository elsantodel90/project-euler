module Problems.Prob36 where
import Elsantodel90.Digits

isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == reverse l

palindromicNumber :: Integer -> Integer -> Bool
palindromicNumber base = isPalindrome . rightToLeftDigitsBase base

doubleBasePalindrome :: Integer -> Bool
doubleBasePalindrome n = palindromicNumber 2 n && palindromicNumber 10 n


answer = sum $ filter doubleBasePalindrome [1..1000000]
