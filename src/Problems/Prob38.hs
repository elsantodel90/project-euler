module Problems.Prob38 where
import Data.List

pandigital :: Integer -> Bool
pandigital n = length s == 9 && length (nub s) == 9 && not ('0' `elem` s)
                where s = show n

answer = maximum [x | k <- [1..9999 :: Integer], n <- [2..9], let x = read . concatMap show $ map (*k) [1..n], pandigital x]
