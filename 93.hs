import Data.List
import Data.Maybe
import Data.Ratio

t1,t2,t3,t4,t5 :: [t] -> [t->t->t] -> t

t1 [a,b,c,d] [f1,f2,f3] = f3 (f1 a b) (f2 c d)
t1 _ _ = error "Cantidades incorrectas t1"

t2 [a,b,c,d] [f1,f2,f3] = f3 (f2 (f1 a b) c) d
t2 _ _ = error "Cantidades incorrectas t2"

t3 [a,b,c,d] [f1,f2,f3] = f3 (f2 a (f1 b c)) d
t3 _ _ = error "Cantidades incorrectas t3"

t4 [a,b,c,d] [f1,f2,f3] = f3 a (f2 (f1 b c) d)
t4 _ _ = error "Cantidades incorrectas t4"

t5 [a,b,c,d] [f1,f2,f3] = f3 a (f2 b (f1 c d))
t5 _ _ = error "Cantidades incorrectas t5"

division :: Maybe Rational -> Maybe Rational -> Maybe Rational
division _ (Just 0) = Nothing
division a b        = direct (/) a b

direct :: (t -> t -> a) -> Maybe t -> Maybe t -> Maybe a
direct _op Nothing _        = Nothing
direct _op _ Nothing        = Nothing
direct op (Just a) (Just b) = Just $ a `op` b

trees :: [[t] -> [t -> t -> t] -> t]
trees = [t1,t2,t3,t4,t5]
ops :: [Maybe Rational -> Maybe Rational -> Maybe Rational]
ops   = [direct (+), direct (-), direct (*), division]

possibleResults :: [Maybe (Ratio Integer)] -> [Integer]
possibleResults l = dropWhile (<1) . map numerator . filter (\x -> denominator x == 1). map head . group . sort $ catMaybes [t p [o1,o2,o3] | p <- permutations l, o1 <- ops, o2 <- ops, o3 <- ops, t <- trees ]
             
value :: Double -> Double -> Double -> Double -> Int
value a b c d = length . takeWhile id . zipWith (==) [1..] . possibleResults $ map (Just . toRational) [a,b,c,d]

main :: IO ()
main = putStrLn . snd $ maximum [(value a b c d, concatMap show [a,b,c,d]) | d <- [0..9], c <- [0..d-1], b <- [0..c-1], a <- [0..b-1]]
