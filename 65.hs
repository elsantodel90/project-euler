import Data.Ratio
import Elsantodel90.Digits
import Elsantodel90.ContinuedFraction

main = print . sumDigits . numerator $ convergent 99 eContinuedFraction

