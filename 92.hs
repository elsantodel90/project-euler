import Data.Array
import Elsantodel90.Digits

f :: Int -> Int
f  = sum . map (^2) . rightToLeftDigits

directLoopClass = head . dropWhile (not . (`elem` [1,89])) . iterate f

maxStored = 1000
loopClassArray = array (1,maxStored) [(i, directLoopClass i) | i <- [1..maxStored]]

loopClass n | n <= maxStored = loopClassArray ! n
            | otherwise      = loopClass (f n)

main = print . length $ filter ((==89) . loopClass) [1..10000000]
