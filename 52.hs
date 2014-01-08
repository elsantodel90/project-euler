import Data.List

sortedDigits :: Integer -> [Char]
sortedDigits = sort . show

magicNumber :: Integer -> Bool
magicNumber x = all ((==sortedDigits x) . sortedDigits) $ map (*x) [2..6]

main :: IO ()
main = print . head $ filter magicNumber [1..]
