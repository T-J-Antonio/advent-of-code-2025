module Main where
import Data.Char (digitToInt, intToDigit)

main = do
    content <- readFile "input.txt"
    let banks = lines content
    let result = sum . map processBank $ banks
    print result
    return ()

processBank :: String -> Int
processBank = processBankRecursiveWithDigit 1 '9'

processBankRecursiveWithDigit :: Int -> Char -> String -> Int
processBankRecursiveWithDigit position digit bank
    | position == 13 = 0
    | length restOfBank > (12 - position) =
        10 ^ (12 - position) * digitToInt digit + processBankRecursiveWithDigit (position + 1) '9' (tail restOfBank)
    | otherwise =
        processBankRecursiveWithDigit position (intToDigit . pred . digitToInt $ digit) bank
    where restOfBank = dropWhile (/= digit) bank
