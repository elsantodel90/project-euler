import Elsantodel90.Primes
import Data.Array.Unboxed
import Data.List

infty :: Integer
infty = 10000000000000000

primes :: [Integer]
primes = map toInteger $ filter (primesUpToArray 100 !) [2..]

-- Upper bound for the number of primes that can appear in the solution
primesBound :: Integer -> Integer
primesBound n = genericLength $ takeWhile (<n) powersOfThree
                  where powersOfThree = iterate (*3) 1

backtrack :: Integer -> Integer -> [Integer] -> Integer -> Integer -> Integer
backtrack minExponent n p divCount acc
    | null p           = if divCount > n then acc else infty
    | l * divCount > n = acc * product p ^ minExponent
    | otherwise        = minimum [seq newAcc $ backtrack i n (tail p) newDivCount newAcc | 
                                (pp, i,pow) <- zip3 primePowers [minExponent..] candidatePowers, 
                                let newAcc = acc * pp,
                                let newDivCount = divCount * pow]
                    where l = (2*minExponent + 1)^(length p)
                          prime = head p
                          candidatePowers = takeWhile (\x -> ((x - 2) ^ length p) * divCount <= n) [2*minExponent+1,2*minExponent+3..]
                          primePowers = iterate (*prime) (prime^minExponent)

firstOverN :: Integer -> Integer
firstOverN n = backtrack 0 (2 * n - 1) (reverse $ genericTake k primes) 1 1
                where k = primesBound (2 * n - 1)

main :: IO ()
main = print $ firstOverN 4000000
