import Elsantodel90.Numeric
import Elsantodel90.IntPot
import Prelude hiding ((^))

solutions :: Integer -> [(Integer,Integer,Integer)]
solutions p = [(a,b,c) | a <- [1..p`div`3], b <- [a..(p-a)`div`2], 
                         let c = integerSqrt 1 (a^2+b^2),
                             c^2 == (a^2 + b^2),
                             a+b+c == p]

solutionCount :: Integer -> Int
solutionCount = length . solutions 
                    
main :: IO ()
main = print . snd $ maximum [(solutionCount p, p) | p <- [1..1000]]
