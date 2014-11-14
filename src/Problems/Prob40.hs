import Data.Char

champerString :: String
champerString = concatMap show [0 :: Int ..]

d :: Int -> Int
d i = ord (champerString !! i) - ord '0'

main :: IO ()
main = print . product . map d . take 7 $ iterate (*10) 1
