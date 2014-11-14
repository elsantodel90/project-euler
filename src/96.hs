import Data.Char

threeDigitNumber :: [[Int]] -> Integer
threeDigitNumber = read . concatMap show . map (+1) . take 3 . head

parseSudoku :: [[Char]] -> [[Int]]
parseSudoku = map (map charVal)
                where charVal  c  = ord c - ord '1'

(!!!) :: [[a]] -> (Int, Int) -> a
mat !!! (i,j) = mat !! i !! j

traspose :: [[a]] -> [[a]]
traspose = map (map head) . takeWhile (not . null . head) . iterate (map tail)

listReplace :: Int -> a -> [a] -> [a]
listReplace i v l = a ++ v:b
                    where (a,_:b) = splitAt i l

matReplace :: (Int, Int) -> a -> [[a]] -> [[a]]
matReplace (i,j) v mat = listReplace i (listReplace j v (mat !! i)) mat

backtracking :: [[Int]] -> [(Int,Int)] -> [[Int]] -> [[Int]] -> [[[Int]]] -> [[[Int]]]
backtracking sudoku nothingCoordinates rows columns squares 
        | null nothingCoordinates = [sudoku]
        | otherwise               = concat [backtracking (matReplace c k sudoku) cs (newRows k) (newColumns k) (newSquares k) | k <- [0..8], available k]
                where c@(i,j):cs = nothingCoordinates
                      rowList    = rows !! i
                      columnList = columns !! j
                      sqc        = ((i `div` 3) , (j `div` 3))
                      squareList = squares !!! sqc
                      forbidden = rowList ++ columnList ++ squareList
                      available = not . (`elem` forbidden)
                      newRows k = listReplace i (k:rowList) rows
                      newColumns k = listReplace j (k:columnList) columns
                      newSquares k = matReplace sqc (k:squareList) squares

solveSudoku :: [[Int]] -> [[Int]]
solveSudoku sudoku = head $ backtracking sudoku nothingCoordinates rows columns squares
                where nothingCoordinates = [(i,j) | i <- [0..8], j <- [0..8], (==(-1)) $ sudoku !!! (i,j)]
                      clean   = filter (>=0)
                      rows    = map clean sudoku
                      columns = map clean $ traspose sudoku
                      squares = map (map clean) $ [[[sudoku !!! (3*i+x,3*j+y) | x <- [0..2], y <- [0..2]] | j <- [0..2]] | i <- [0..2]]

main :: IO ()
main = readFile "96.in" >>= print . sum . map (threeDigitNumber . solveSudoku . parseSudoku . tail) . takeWhile (not . null) . map (take 10) . iterate (drop 10) . lines
