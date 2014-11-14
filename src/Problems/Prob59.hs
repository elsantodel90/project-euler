module Problems.Prob59 where
import Data.Bits
import Data.Char
import Elsantodel90.Parsing


-- Usando este main, vemos las secuencias de 3 bien alineadas con la clave.
-- Tomamos la primera (la que mas aparece) como candidateForThe (candidato para la palabra "the")
{-
main :: IO ()
main = readFile "59.in" >>= mapM_ print . take 10 . sortBy cmp . group . sort . groups 3 . split ',' . init
    where groups k = map (take k) . takeWhile (not . null) . iterate (drop k)
          cmp a b = flip compare (length a) (length b)
-}

candidateForThe :: [Int]
candidateForThe = [19,7,1]

candidateKey :: [Int]
candidateKey = zipWith xor (map ord "the") candidateForThe

decipher :: [Int] -> [Int] -> String
decipher key = map chr . zipWith xor (cycle key)

answer_calculator :: IO Int
answer_calculator = readFile "59.in" >>= return . sum . map ord . decipher candidateKey . map read . split ',' . init
