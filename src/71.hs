import Data.Ratio
import Elsantodel90.BinarySearch

largestN :: Integer -> Integer
largestN d = fst $ binarySearch (\n -> n % d < 3 % 7) 0 d

main :: IO ()
main = print . numerator $ maximum [largestN d % d | d <- [1..1000000]]
