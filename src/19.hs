import Data.List

data YearKind = Leap | Normal

monthDays :: Integer -> [Integer]
monthDays year = case yearKind year of  
                    Leap   -> [31,29,31,30,31,30,31,31,30,31,30,31]
                    Normal -> [31,28,31,30,31,30,31,31,30,31,30,31]

yearKind :: Integer -> YearKind
yearKind n | n `mod` 400 == 0 = Leap
           | n `mod` 100 == 0 = Normal
           | n `mod`   4 == 0 = Leap
           | otherwise        = Normal

yearDays :: Integer -> Integer
yearDays =  sum . monthDays

-- Epoch is 1 Jan 1900 (Monday)
-- month starts at 0 for January
-- Same for days: 0 is the first day of month
daysFromEpoch :: Integer -> Integer -> Integer -> Integer
daysFromEpoch year month day = yearlyDays + monthlyDays + day
                                where yearlyDays  = sum $ map yearDays [1900..year-1]
                                      monthlyDays = sum . genericTake month $ monthDays year

wasSunday :: Integer -> Integer -> Integer -> Bool
wasSunday year month day = daysFromEpoch year month day `mod` 7 == 6

twentiethCenturySundays :: [(Integer, Integer)]
twentiethCenturySundays = [(year,month) | year <- [1901..2000], month <- [0..11], wasSunday year month 0]

main :: IO ()
main = print $ length twentiethCenturySundays
