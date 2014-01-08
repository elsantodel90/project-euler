import Data.Bits
import Data.Array

num :: Integer
num  = 10^25
n    = length . takeWhile (/=0) $ iterate (`shiftR` 1) num

beta True  = 1
beta False = 0

dpArray = array ((0,False), (n, True)) [((i,c), dp i c) | i <- [0..n], c <- [False,True]]

dp i c | i == n    = beta (not c)
       | otherwise = if c == b 
                        then dpArray ! ((i+1),False) + dpArray ! ((i+1),True)
                        else dpArray ! ((i+1),c)
            where b = testBit num i

main = print $ dpArray ! (0,False)

