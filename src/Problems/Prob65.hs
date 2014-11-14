module Problems.Prob65 where
import Data.Ratio
import Elsantodel90.Digits
import Elsantodel90.ContinuedFraction


answer = (sumDigits :: Integer -> Integer) . numerator $ convergent 99 eContinuedFraction

