-- Advent of code 2023 Day 1
-- main program get input data from inputfile file

import Data.Char (ord, isDigit)

inputfile = "input_1.txt"

helperFunc :: Maybe (Char, Char) -> Char -> Maybe (Char, Char)
helperFunc Nothing c = if isDigit c then Just (c, c) else Nothing
helperFunc (Just (a, b)) c = if isDigit c then Just (a, c) else Just (a, b)

helperFunc' :: Maybe (Char, Char) -> Char -> Maybe (Char, Char)
helperFunc' Nothing c = if isDigit c then Just (c, c) else Nothing
helperFunc' (Just (a, b)) c = if isDigit c then Just (a, c) else Just (a, b)

fromTupletoInt :: Maybe (Char, Char) -> Int
fromTupletoInt Nothing = 0
fromTupletoInt (Just (a, b)) = 10 * (ord a - 48) + ord b - 48

calibrationValue :: String -> Int
calibrationValue inputLine = fromTupletoInt $ foldl helperFunc Nothing inputLine

calibrationValue' :: String -> Int
calibrationValue' inputLine = fromTupletoInt $ foldl helperFunc' Nothing inputLine

sumCalibrationValues input = sum $ map calibrationValue (lines input)

sumCalibrationValues' input = sum $ map calibrationValue' (lines input)

main :: IO ()
main = do
    input <- readFile inputfile
    putStrLn $ show $ sumCalibrationValues input
    putStrLn $ show $ sumCalibrationValues' input
