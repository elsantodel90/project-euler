import Data.List
import Data.Array.Unboxed
import Elsantodel90.Primes

---  Observaciones clave:
---  >>>  Notar que todos los k numeros dividen al sum!!!!! De hecho forman una partition del numero.
---  >>>  No representar explicitamente los 1 : Son muchos y aportan poco

fArray :: UArray Int Int
fArray = factorArray 24000
sortedDivisors :: Int -> [Int]
sortedDivisors = tail . sort . divisorsUsingArray fArray

partitions :: Int -> [[Int]]
partitions n = partitionsWithDivs (sortedDivisors n) n

partitionsWithDivs :: [Int] -> Int -> [[Int]]
partitionsWithDivs _divisors 1 = [[]]
partitionsWithDivs  divisors n = [a : p | ndiv <- takeWhile (not . null) (iterate tail divisors), 
                                         let a = head ndiv, 
                                         n `mod` a == 0,
                                         p <- partitionsWithDivs ndiv (n `div` a)]

kGood :: Int -> Int -> Bool
kGood k n = any goodPartition $ partitions n
              where goodPartition part = n - sum part + length part == k
minimalGood :: Int -> Int
minimalGood k = head $ filter (kGood k) [k..]

sortUniq :: [Int] -> [Int]
sortUniq = map head . group . sort

main :: IO ()
main = print . sum . sortUniq $ map minimalGood [2..12000]
                       
