module Main where
import Data.Char (digitToInt, intToDigit)

main = do
    content <- readFile "input.txt"
    let banks = lines content
    let result = sum . map processBank $ banks
    print result
    return ()

processBank :: String -> Int
processBank = processBankWithDigit '9'

processBankWithDigit :: Char -> String -> Int
processBankWithDigit digit bank
    | length restOfBank > 1 = 10 * digitToInt digit + processBankWithDigit2 '9' (tail restOfBank)
    | otherwise = processBankWithDigit (intToDigit . pred . digitToInt $ digit) bank
    where restOfBank = dropWhile (/= digit) bank

processBankWithDigit2 :: Char -> String -> Int
processBankWithDigit2 digit bank
    | digit `elem` bank = digitToInt digit
    | otherwise = processBankWithDigit2 (intToDigit . pred . digitToInt $ digit) bank
