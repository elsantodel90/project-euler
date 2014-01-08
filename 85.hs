target = 2000000
diffUpperBound = abs $ f 66 43 - target

f a b = (a * (a+1) * b * (b+1)) `div` 4

tries a = takeWhile (\(_,cant,_) -> cant <= target + diffUpperBound) [(abs (cant - target), cant, a*b) | b <- [1..], let cant = f a b]

main = print . (\(_,_,area) -> area) . minimum . concat . takeWhile (not . null) $ map tries [1..]
