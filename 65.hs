import Data.Ratio
import Elsantodel90.Digits
import Elsantodel90.ContinuedFraction

main :: IO ()
main = print . (sumDigits :: Integer -> Integer) . numerator $ convergent 99 eContinuedFraction

