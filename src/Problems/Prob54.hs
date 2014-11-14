module Problems.Prob54 where
import Data.Char
import Data.List
import Data.Maybe

data Card = Card { value ::Int , suit :: Char } 
type HandScore = Maybe [Int]
type Hand = [Card]
type ScoringFunction = Hand -> HandScore
type Game = (Hand,Hand) 

handValues :: Hand -> [Int]
handValues = map value
handSuits :: Hand -> [Char]
handSuits  = map suit

compareOn :: Ord a => (t -> a) -> t -> t -> Ordering
compareOn f a b = compare (f a) (f b)

turnAcesToOnes :: Int -> Int
turnAcesToOnes x | x == parseValue 'A' = 1
                 | otherwise           = x

sortDecreasingBy :: (a -> a -> Ordering) -> [a] -> [a]
sortDecreasingBy = sortBy . flip
sortDecreasing :: Ord a => [a] -> [a]
sortDecreasing = sortDecreasingBy compare
sortedValues :: Hand -> [Int]
sortedValues = sortDecreasing . handValues
groups :: Int -> Hand -> [[Int]]
groups k = filter (\g -> length g >= k) . group . sortedValues
highCard :: ScoringFunction
highCard  = Just . sortedValues
kind :: Int -> ScoringFunction
kind k hand = fmap ( ++ sortedValues hand) . listToMaybe $ groups k hand
onePair :: ScoringFunction
onePair =  kind 2
twoPairs :: ScoringFunction
twoPairs hand = if length pairs >= 2 then Just (map head pairs)  else Nothing
                  where pairs = groups 2 hand
threeOfAKind :: ScoringFunction
threeOfAKind = kind 3
straightOf :: Int -> [Int]
straightOf = take 5 . iterate (subtract 1)
straightValues :: [Int] -> Maybe [Int]
straightValues values = listToMaybe $ catMaybes [val values, val (map turnAcesToOnes values)]
                          where val v | sortDecreasing v == straightOf (maximum v) = Just $ [maximum v]
                                      | otherwise = Nothing
straight :: ScoringFunction
straight = straightValues . handValues
flush :: ScoringFunction
flush hand | length (nub $ handSuits hand) == 1 = Just $ sortedValues hand
           | otherwise                      = Nothing
fullHouse :: ScoringFunction
fullHouse hand | map length groupList == [3,2] = Just $ map head groupList
               | otherwise                     = Nothing
                   where groupList = sortDecreasingBy (compareOn length) $ groups 2 hand
fourOfAKind :: ScoringFunction
fourOfAKind = kind 4
straightFlush :: ScoringFunction
straightFlush hand = straight hand >> flush hand
gameScorers :: [ScoringFunction]
gameScorers = straightFlush:
              fourOfAKind:
              fullHouse:
              flush:
              straight:
              threeOfAKind:
              twoPairs:
              onePair:
              highCard:
              []

parseValue :: Char -> Int
parseValue 'A' = 14
parseValue 'K' = 13
parseValue 'Q' = 12
parseValue 'J' = 11
parseValue 'T' = 10
parseValue c   = ord c - ord '0'
parseCard :: String -> Card
parseCard [v, s] = Card (parseValue v) s
parseCard cardString = error $ "Parse Error: card " ++ cardString ++ " cannot be parsed as a valid card."

parseGame :: String -> Game
parseGame = splitAt 5 . map parseCard . words

score :: Hand -> [Int]
score hand = head . catMaybes . zipWith (\x m -> fmap (x:) m) [1,0..] $ map ($hand) gameScorers

player1Wins :: Game -> Bool
player1Wins (hand1, hand2) = score hand1 > score hand2

answer_calculator :: IO Int
answer_calculator = readFile "54.in" >>= return . length . filter player1Wins . map parseGame . lines
