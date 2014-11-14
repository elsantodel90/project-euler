import Elsantodel90.Primes

-- Pasamos a Integer para hacer la suma para evitar overflows
main :: IO ()
main = print . sum . map toInteger $ primesUpTo 2000000
