import Data.List

allEqual :: Eq a => [a] -> Bool
allEqual (x:xs) = all (==x) xs
allEqual _      = error "ERROR: allEqual espera una lista no vacia!"

groups :: [t] -> [[t]]
groups [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] = [x0,x1,x2]:
                                         [x3,x2,x4]:
                                         [x5,x4,x6]:
                                         [x7,x6,x8]:
                                         [x9,x8,x1]:
                                         []
groups _ = error "ERROR: Deberian ser 10 elementos!"

magic5gon :: [Integer] -> Bool
magic5gon = allEqual . map sum .  groups

rep :: [Integer] -> [Integer]
rep l = concat . take 5 . dropWhile (/= minGroup) $ cycle g
            where g        = groups l
                  minGroup = minimum g

main :: IO ()    
main = putStrLn . maximum . filter ( (==16) . length) . map (concatMap show . rep) . filter magic5gon $ permutations [1..10]
