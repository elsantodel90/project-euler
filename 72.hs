import Elsantodel90.Primes

top = 1000000
phi = phiUsingArray $ factorArray top

main = print . sum $ map (toInteger . phi) [2..top]
