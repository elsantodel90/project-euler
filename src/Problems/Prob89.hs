import Elsantodel90.RomanNumerals

savedCharacters :: String -> Int
savedCharacters numeral = length numeral - length (normalizedNumeral numeral)

main :: IO ()
main = readFile "89.in" >>= print . sum . map savedCharacters  . lines
