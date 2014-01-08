import Data.List

allEqual (x:xs) = all (==x) xs

groups [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] = [x0,x1,x2]:
                                         [x3,x2,x4]:
                                         [x5,x4,x6]:
                                         [x7,x6,x8]:
                                         [x9,x8,x1]:
                                         []
magic5gon = allEqual . map sum .  groups

rep l = concat . take 5 . dropWhile (/= minGroup) $ cycle g
            where g        = groups l
                  minGroup = minimum g
    
main = putStrLn . maximum . filter ( (==16) . length) . map (concatMap show . rep) . filter magic5gon $ permutations [1..10]
