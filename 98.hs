import Data.List
import Data.Maybe
import Elsantodel90.Parsing
import Elsantodel90.Numeric
import Elsantodel90.IntPot
import Prelude hiding ((^))

isSquare :: Integer -> Bool
isSquare n = integerSqrt 1 n ^ 2 == n

sortedAssignments :: Integer -> [t] -> [[(t, Integer)]]
sortedAssignments _ [] = [[]]
sortedAssignments 10 _ = []
sortedAssignments minDigit l@(c:cs) = map ((c, minDigit):) (sortedAssignments (minDigit+1) cs) ++ sortedAssignments (minDigit+1) l

assignments :: [t] -> [[(t, Integer)]]
assignments = concatMap (sortedAssignments 0) . permutations

lookup' :: Eq a => a -> [(a,b)] -> b
lookup' x l = fromJust $ lookup x l

value :: [(Char,Integer)] -> String -> Integer
value d l = foldl' (\x u -> 10*x + u) 0 $ map (flip lookup' d) l

largestSquare :: String -> String -> Maybe Integer
largestSquare a b = if null list then Nothing else Just (maximum list)
                    where letters = nub a
                          list    =     [max aVal bVal | d <- assignments letters, 
                                                    lookup' (head a) d /= 0, 
                                                    lookup' (head b) d /= 0,
                                                    let aVal = value d a, isSquare aVal,
                                                    let bVal = value d b, isSquare bVal]
largestSquareFromList :: [String] -> Maybe Integer
largestSquareFromList l = maximum [largestSquare a b | a <- l, b <- l, a < b]
             
solve :: [String] -> Integer
solve = head . catMaybes . map (largestSquareFromList . snd) . sortBy (flip compare) . map (\l -> (length (snd $ head l), map snd l)) . filter ((>=2) . length). groupBy (\a b -> fst a == fst b) . sort . map (\word -> (sort word , word))

main :: IO ()
main = readFile "98.in" >>= print . solve . wordListLineRead
