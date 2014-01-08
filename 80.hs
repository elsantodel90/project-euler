import Elsantodel90.Numeric
import Elsantodel90.Digits

isSquare n = (==n) . head . dropWhile (<n) $ map (^2) [1..]

total n = sum . take 100 . leftToRightDigits $ integerSqrt 1 (10^200 * n)

main = print . sum . map total $ filter (not . isSquare) [1..100]
