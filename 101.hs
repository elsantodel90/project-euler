import Data.Ratio
import Elsantodel90.Polynomial

polySequence polynomial = map (eval polynomial) [1..]

nicePolynomial :: Polynomial Rational
nicePolynomial = sum [(-1)^n * xPol^n | n <- [0..10]]
nicePolySequence = polySequence nicePolynomial

op k = interpolating . take k $ zip [1..] nicePolySequence
bop k = kSeq !! (length . takeWhile id $ zipWith (==) nicePolySequence kSeq)
        where kSeq = polySequence $ op k

main = print . sum $ map (numerator . bop) [1..degree nicePolynomial]
