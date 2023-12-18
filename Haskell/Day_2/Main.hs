-- Advent of code 2023 Day 1
-- main program get input data from inputfile file

import Data.Char (ord, isDigit)
import Text.ParserCombinators.ReadP

inputfile = "input_1.txt"

data Cube = Red Int | Green Int | Blue Int 
    deriving (Eq, Ord)

instance Show Cube where
    show (Red i) = show i ++ " red"
    show (Green i) = show i ++ " green"
    show (Blue i) = show i ++ " blue"
    
-- instance Read Cube where
--    readsPrec _ input =

digit :: ReadP Char
digit =
    satisfy (\char -> char >= '0' && char <= '9')

integer :: ReadP Cube
integer = do
    num <- fmap read $ munch1 isDigit
    optional (char ' ')
    color <- string "red" +++ string "green" +++ string "blue"
    case color of
        "red" -> return (Red num)
        "green" -> return (Green num)
        "blue" -> return (Blue num)



-- instance Eq Cube where
--     (==) (Red i) (Red j) = i == j
--     (==) (Green i) (Green j) = i == j
--     (==) (Blue i) (Blue j) = i == j
--     (==) _ _ = False

-- instance Ord Cube where
--     (<=) (Red i) (Red j) = i == j
--     (==) (Green i) (Green j) = i == j
--     (==) (Blue i) (Blue j) = i == j
--     (==) _ _ = False

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
