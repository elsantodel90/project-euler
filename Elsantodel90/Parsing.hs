module Elsantodel90.Parsing(split, wordListLineRead) where

split :: Eq a => a -> [a] -> [[a]]
split c s | null s    = []
          | otherwise = next : split c (drop 1 rest)
                    where (next, rest) = span (/= c) s

wordListLineRead :: String -> [String]
wordListLineRead = split ',' . filter (/= '"') . init
