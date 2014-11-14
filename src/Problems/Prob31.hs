module Problems.Prob31 where
import Data.Array

coins :: [Int]
coins = [1,2,5,10,20,50,100,200]

ways :: Array (Int, Int) Integer
ways = array ((0,0),(200, length coins)) $ 
                 [ ((0,k),1) | k <- [0..length coins]] ++
                 [ ((n,length coins),0) | n <- [1..200]] ++
                 [ ((n,k), ways ! (n,k+1) + if coin <= n then ways ! (n-coin,k) else 0)  | n <- [1..200], k <- [0..length coins - 1], let coin = coins !! k]
                 
answer = ways ! (200,0)
