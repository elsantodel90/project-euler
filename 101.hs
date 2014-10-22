import Data.Ratio
import Elsantodel90.Polynomial

polySequence :: Polynomial Rational -> [Rational]
polySequence polynomial = map (eval polynomial) [1..]

nicePolynomial :: Polynomial Rational
nicePolynomial = sum [(-1)^n * xPol^n | n <- [0..10 :: Integer]]
nicePolySequence :: [Rational]
nicePolySequence = polySequence nicePolynomial

op :: Int -> Polynomial Rational
op k = interpolating . take k $ zip [1..] nicePolySequence
bop :: Int -> Rational
bop k = kSeq !! (length . takeWhile id $ zipWith (==) nicePolySequence kSeq)
        where kSeq = polySequence $ op k

main :: IO ()
main = print . sum $ map (numerator . bop) [1..degree nicePolynomial]
