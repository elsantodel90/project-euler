import Data.Ratio
import Elsantodel90.Digits

terms :: [Rational]
terms = map (+1) $ iterate f (1 % 2)
            where f x = 1 / (2 + x)
            
good :: Rational -> Bool
good x = digits p > digits q
            where p = numerator x
                  q = denominator x

main :: IO ()
main = print . length . filter good $ take 1000 terms 
