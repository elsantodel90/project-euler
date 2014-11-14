module Problems.Prob52 where
import Data.List

sortedDigits :: Integer -> [Char]
sortedDigits = sort . show

magicNumber :: Integer -> Bool
magicNumber x = all ((==sortedDigits x) . sortedDigits) $ map (*x) [2..6]


answer = head $ filter magicNumber [1..]
