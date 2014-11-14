import Data.Ratio

magic :: Integer -> Integer -> Integer -> Integer -> Bool
magic a b c d = cancelable && not (b == 0 && d == 0)
                        where check x y = (10*a + b)*y  == (10*c + d) * x
                              cancelable = (a == c  && check b d) ||
                                           (a == d  && check b c) ||
                                           (b == c  && check a d) ||
                                           (b == d  && check a c)

main :: IO ()
main = print . denominator $ product [x % y | a <- [1..9], b <- [0..9], c <- [1..9], d <- [0..9], magic a b c d,
                                        let x = 10*a+b, let y = 10*c+d, x < y]
