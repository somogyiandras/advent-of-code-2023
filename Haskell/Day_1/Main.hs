-- Advent of code 2023 Day 1
-- main program get input data from inputfile file

import Data.Char (ord, isDigit)
inputfile = "input_1.txt"

fromTupletoInt :: Maybe (Char, Char) -> Int
fromTupletoInt Nothing = 0
fromTupletoInt (Just (a, b)) = 10 * (ord a - 48) + ord b - 48

-- 1st star

helperFunc :: Maybe (Char, Char) -> Char -> Maybe (Char, Char)
helperFunc Nothing c = if isDigit c then Just (c, c) else Nothing
helperFunc (Just (a, b)) c = if isDigit c then Just (a, c) else Just (a, b)

calibrationValue :: String -> Int
calibrationValue = fromTupletoInt . foldl helperFunc Nothing

sumCalibrationValues input = sum $ map calibrationValue (lines input)

-- 2nd star

helperFunc' :: Maybe (Char, Char) -> String -> Maybe (Char, Char)
helperFunc' Nothing ('o':'n':'e':r) = helperFunc' (Just ('1', '1')) ('e':r)
helperFunc' Nothing ('t':'w':'o':r) = helperFunc' (Just ('2', '2')) ('o':r)
helperFunc' Nothing ('t':'h':'r':'e':'e':r) = helperFunc' (Just ('3', '3')) ('e':r)
helperFunc' Nothing ('f':'o':'u':'r':r) = helperFunc' (Just ('4', '4')) ('r':r)
helperFunc' Nothing ('f':'i':'v':'e':r) = helperFunc' (Just ('5', '5')) ('e':r)
helperFunc' Nothing ('s':'i':'x':r) = helperFunc' (Just ('6', '6')) ('x':r)
helperFunc' Nothing ('s':'e':'v':'e':'n':r) = helperFunc' (Just ('7', '7')) ('n':r)
helperFunc' Nothing ('e':'i':'g':'h':'t':r) = helperFunc' (Just ('8', '8')) ('t':r)
helperFunc' Nothing ('n':'i':'n':'e':r) = helperFunc' (Just ('9', '9')) ('e':r)
helperFunc' Nothing (c:r) = if isDigit c
                            then helperFunc' (Just (c, c)) r
                            else helperFunc' Nothing r
helperFunc' (Just (a, b)) [] = Just (a, b)
helperFunc' (Just (a, _)) ('o':'n':'e':r) = helperFunc' (Just (a, '1')) ('e':r)
helperFunc' (Just (a, _)) ('t':'w':'o':r) = helperFunc' (Just (a, '2')) ('o':r)
helperFunc' (Just (a, _)) ('t':'h':'r':'e':'e':r) = helperFunc' (Just (a, '3')) ('e':r)
helperFunc' (Just (a, _)) ('f':'o':'u':'r':r) = helperFunc' (Just (a, '4')) ('r':r)
helperFunc' (Just (a, _)) ('f':'i':'v':'e':r) = helperFunc' (Just (a, '5')) ('e':r)
helperFunc' (Just (a, _)) ('s':'i':'x':r) = helperFunc' (Just (a, '6')) ('x':r)
helperFunc' (Just (a, _)) ('s':'e':'v':'e':'n':r) = helperFunc' (Just (a, '7')) ('n':r)
helperFunc' (Just (a, _)) ('e':'i':'g':'h':'t':r) = helperFunc' (Just (a, '8')) ('t':r)
helperFunc' (Just (a, _)) ('n':'i':'n':'e':r) = helperFunc' (Just (a, '9')) ('e':r)
helperFunc' (Just (a, b)) (c:r) = if isDigit c
                            then helperFunc' (Just (a, c)) r
                            else helperFunc' (Just (a, b)) r

calibrationValue' :: String -> Int
calibrationValue' inputLine = fromTupletoInt $ helperFunc' Nothing inputLine

sumCalibrationValues' input = sum $ map calibrationValue' (lines input)

main :: IO ()
main = do
    input <- readFile inputfile
    putStrLn $ "Result 1st star: " ++ (show $ sumCalibrationValues input)
    putStrLn $ "Result 2nd star: " ++ (show $ sumCalibrationValues' input)
