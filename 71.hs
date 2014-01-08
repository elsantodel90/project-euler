import Data.Ratio
import Elsantodel90.BinarySearch

largestN d = fst $ binarySearch (\n -> n % d < 3 % 7) 0 d

main = print . numerator $ maximum [largestN d % d | d <- [1..1000000]]
