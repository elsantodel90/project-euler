module Problems.Prob56 where
import Elsantodel90.Digits
import Elsantodel90.IntPot
import Prelude hiding ((^))

answer = maximum [sumDigits (a^b) | a <- [1..99 :: Integer], b <- [1..99]]
