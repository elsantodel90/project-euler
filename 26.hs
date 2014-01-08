import Data.List
import Data.Maybe

--s n k = (n*10)`div`k: s ((n*10) `mod` k) k

r :: Integer -> Integer -> [Integer]
r n k = n: r ((n*10) `mod` k) k

primerRep :: [Integer] -> [Integer] -> Int -> Int
primerRep [] _ _ = error "La lista de residuos no se deberia acabar!!!!!!!"
primerRep (x:xs) acum c | x `elem` acum = c
                        | otherwise     = primerRep xs (x:acum) (c+1)
l :: Integer -> Int
l k = pRep - first
        where lista = r 1 k
              pRep  = primerRep lista [] 0
              rNum  = lista !! pRep
              first = fromJust $ findIndex (==rNum) lista

main :: IO ()
main = print . snd . maximum $ [ (l i, i) | i <- [1..999]]
