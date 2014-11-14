import Data.List
import Data.Array.Unboxed
import Elsantodel90.Primes


isPrime :: Int -> Bool
isPrime = (primesUpToArray 10000 !)

allArePermutations :: [Int] -> Bool
allArePermutations = (==1) . length . nub . map (sort . show)

solutions :: [[Int]]
solutions = [ l | base <- [1000..9999], inc <- [1..4500], let s = [base,base+inc..9999], 
                  let l = take 3 s,
                  length l == 3,
                  all isPrime l,
                  allArePermutations l]

main :: IO ()
main = putStrLn . concatMap show $ solutions !! 1
