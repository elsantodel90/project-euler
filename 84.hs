import Data.List
import Text.Printf
import Elsantodel90.Numeric

-- Usamos una cadena de markov a la cual le resolvemos el sistema de ecuaciones asociado con eliminacion gaussiana :)
-- Ignoramos la parte de que "lo de arriba va a abajo" y etc, o sea asumimos que al sacar una carta, las 16 del pilon
-- son equiprobables como si shuflearamos todo todo el tiempo. El error que esto introduce es minimo (se puede ver en el
-- ejemplo del enunciado con dados de 6).

cc, ch :: [Int]
cc   = [2, 17, 33]
ch   = [7, 22, 36]
jail, goToJail, go, c1, e3, h2, r1, r2, r3, r4, u1, u2 :: Int
jail = 10
goToJail = 30
go = 0
c1 = 11
e3 = 24
h2 = 39
r1 = 5
r2 = 15
r3 = 25
r4 = 35
railways :: [Int]
railways = [r1,r2,r3,r4]
u1 = 12
u2 = 28
utilities :: [Int]
utilities = [u1,u2]

advance :: Int -> Int -> Int
advance k x = (x+k) `mod` 40
next :: [Int] -> Int -> Int
next l = head . filter (`elem` l) . iterate (advance 1)
nextRailway :: Int -> Int
nextRailway = next railways
nextUtility :: Int -> Int
nextUtility = next utilities

basicLanding :: Int -> [(Int, Double)]
basicLanding x | elem x cc = [(jail,u), (go, u), (x, 14*u)]
               | elem x ch = [(jail,u), (go, u), (c1, u), (e3, u), (h2, u), 
                          (r1, u), (nextRailway x, 2*u), (nextUtility x, u), ((x-3)`mod`40, u),
                          (x, 6*u)]
               | x == goToJail = [(jail, 1)]
               | otherwise = [(x,1)]
                    where u = 1 / 16

packId :: Int -> Int -> Int
packId squareId doubleCount | doubleCount == 3 = packId jail 0
                            | otherwise        = 40 * doubleCount + squareId
                            
unpackId :: Int -> (Int, Int)
unpackId x = (x `mod` 40, x `div` 40)

packedAdvance :: (Int, Int) -> Int -> Int
packedAdvance (d1,d2) x = packId (advance (d1+d2) s) doubleCount
                    where (s,c) = unpackId x
                          doubleCount = if d1 == d2 then c+1 else 0

landing :: Int -> [(Int, Double)]
landing x = map adjust $ basicLanding squareId
                where (squareId,doubleCount) = unpackId x
                      adjust (s,p) = (packId s doubleCount, p)

dice :: Int -> [(Int,Int, Double)]
dice n = [(i,j, 1 / fromIntegral (n*n)) | i <- [1..n], j <- [1..n]]

directTransitions :: Int -> [(Int,Int, Double)]
directTransitions n = [(c,a,pb * p)  | a <- [0..119], (d1,d2,pb) <- dice n, (c,p) <- landing (packedAdvance (d1,d2) a)]

transitions :: Int -> [(Int, Int, Double)]
transitions = map add . groupBy (\(a,b,_) (c,d,_) -> a == c && b == d) . sort . directTransitions
                where add l@((a,b,_) : _) = (a,b, sum $ map (\(_,_,c) -> c) l)
                      add _               = error "NO DEBERIA PASAR!!"

equationForAt :: Int -> Int -> [(Int, Int, Double)] -> [Double]
equationForAt n i l@((_,from, p):rest) | i == 120   = []
                                       | from == i = nFactor + p : equationForAt n (i+1) rest
                                       | otherwise = nFactor     : equationForAt n (i+1) l
                                            where nFactor    = if n    == i then -1.0 else 0.0
equationForAt _ _ _ = error "No deberia pasar esto!!!"

equation :: Int -> [(Int, Int, Double)] -> [Double]
equation n l = equationForAt n 0 (l ++ [(n,1000,0)] ) ++ [0.0]
         

equationsAt :: Int -> [[(Int, Int, Double)]] -> [[Double]]
equationsAt i g@(l:ls) | i == 120   = []
                       | n == i    = equation i  l : equationsAt (i+1) ls
                       | otherwise = equation i [] : equationsAt (i+1) g
                           where (n,_,_) = head l
equationsAt _ _ = error "No deberia pasar esto!!!"

equations :: [[(Int, Int, Double)]] -> [[Double]]
equations groups = equationsAt 0 (groups ++ [[(1000,0,0)]])

compact :: [Double] -> [Double]
compact l = zipWith3 (\a b c -> a+b+c) l1 l2 l3
              where (l1,rest) = splitAt 40 l
                    (l2,l3)   = splitAt 40 rest

main :: IO ()
main = putStrLn . concatMap (printf "%0.2d" :: Int -> String) . map snd . take 3 . sortBy (flip compare) 
                . flip zip [0..] . compact . solveLinearSystem . (replicate 121 1.0:) . tail . equations 
                . groupBy (\(a,_,_) (b,_,_) -> a == b) $ transitions 4
