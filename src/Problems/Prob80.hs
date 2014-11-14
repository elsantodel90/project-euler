import Elsantodel90.Numeric
import Elsantodel90.Digits
import Elsantodel90.IntPot
import Prelude hiding ((^))

isSquare :: Integer -> Bool
isSquare n = (==n) . head . dropWhile (<n) $ map (^2) [1..]

total :: Integer -> Integer
total n = sum . take 100 . leftToRightDigits $ integerSqrt 1 (10^200 * n)

main :: IO ()
main = print . sum . map total $ filter (not . isSquare) [1..100]
