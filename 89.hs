import Elsantodel90.RomanNumerals

savedCharacters numeral = length numeral - length (normalizedNumeral numeral)

main = readFile "89.in" >>= print . sum . map savedCharacters  . lines
