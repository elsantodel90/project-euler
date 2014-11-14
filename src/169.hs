import Data.Bits
import Data.Array
import Elsantodel90.IntPot
import Prelude hiding ((^))

num :: Integer
num  = 10^25
n :: Int
n    = length . takeWhile (/=0) $ iterate (`shiftR` 1) num

beta :: Bool -> Integer
beta True  = 1
beta False = 0

dpArray :: Array (Int, Bool) Integer
dpArray = array ((0,False), (n, True)) [((i,c), dp i c) | i <- [0..n], c <- [False,True]]

dp :: Int -> Bool -> Integer
dp i c | i == n    = beta (not c)
       | otherwise = if c == b 
                        then dpArray ! ((i+1),False) + dpArray ! ((i+1),True)
                        else dpArray ! ((i+1),c)
            where b = testBit num i

main :: IO ()
main = print $ dpArray ! (0,False)

