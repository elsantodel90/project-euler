import Data.Array.Unboxed
import Elsantodel90.Primes

maxPrime :: Int
maxPrime = 1000000

primesArray :: UArray Int Bool
primesArray = primesUpToArray maxPrime

superRead :: String -> Int
superRead s = if null s then 0 else read s

removeFirstDigit :: Int -> Int
removeFirstDigit = superRead . tail . show

rightTruncatable :: Int -> Bool
rightTruncatable = all (primesArray !) . takeWhile (/= 0) . iterate (`div` 10)

leftTruncatable :: Int -> Bool
leftTruncatable  = all (primesArray !) . takeWhile (/= 0) . iterate removeFirstDigit

truncatable :: Int -> Bool
truncatable n = leftTruncatable n && rightTruncatable n

main :: IO ()
main = print . sum $ filter truncatable [11..maxPrime]
