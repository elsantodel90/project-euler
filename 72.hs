import Elsantodel90.Primes

top :: Int
top = 1000000
phi :: Int -> Int
phi = phiUsingArray $ factorArray top

main :: IO ()
main = print . sum $ map (toInteger . phi) [2..top]
