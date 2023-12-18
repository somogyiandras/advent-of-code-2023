-- Advent of code 2023 Day 1
-- main program get input data from inputfile file

import Data.Char (ord, isDigit)
import Text.ParserCombinators.ReadP

inputfile = "input_2.txt"
criteria = [Red 12, Green 13, Blue 14]


data Cube = Red Int | Green Int | Blue Int 
    deriving (Eq, Ord)

instance Show Cube where
    show (Red i) = show i ++ " red"
    show (Green i) = show i ++ " green"
    show (Blue i) = show i ++ " blue"
    
-- instance Read Cube where
--    readsPrec _ input =

parsSep :: ReadP Char
parsSep = do
    skipSpaces
    sep <- char ',' +++ char ';'
    skipSpaces
    return sep

parsDigit :: ReadP Char
parsDigit =
    satisfy (\char -> char >= '0' && char <= '9')

parsCube :: ReadP Cube
parsCube = do
    num <- fmap read $ munch1 isDigit
    optional (char ' ')
    color <- string "red" +++ string "green" +++ string "blue"
    case color of
        "red" -> return (Red num)
        "green" -> return (Green num)
        "blue" -> return (Blue num)

parsHead :: ReadP Int
parsHead = do
    string "Game "
    num <- fmap read $ munch1 isDigit
    string ": "
    return num

parsLine :: ReadP (Int, [Cube])
parsLine = do
    skipSpaces
    id <- parsHead
    listCube <- sepBy parsCube parsSep
    return (id, listCube)
    
-- the fully parsed line fst $ last $ readP_to_S parsLine "line"
-- but now we need only the line number for which the games are possible

parsPossible :: ReadP (Int)
parsPossible = do
    skipSpaces
    id <- option 0 parsHead
    possible <- sepBy parsCube parsSep
    return (id)

isPossible :: String -> Int
isPossible inputLine = fst $ last $ readP_to_S parsPossible inputLine

sumPossible input = sum $ map isPossible (lines input)

main :: IO ()
main = do
    input <- readFile inputfile
    putStrLn $ show $ sumPossible input
