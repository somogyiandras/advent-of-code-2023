-- Advent of code 2023 Day 1
-- main program get input data from inputfile file

import Data.Char (isDigit)
import ParsCube
import Text.ParserCombinators.ReadP

inputfile = "input_2.txt"
criteria = [Red 12, Green 13, Blue 14]

-- ParsCube modul parses Cube type
-- but now we need only the line number for which the games are possible

-- compCrit :: [Cube] -> Bool
-- compCrit cs = 

parsPossibleCube :: ReadP (Bool)
parsPossibleCube = do
    num <- fmap read $ munch1 isDigit
    optional (char ' ')
    color <- string "red" +++ string "green" +++ string "blue"
    cube <- case color of
        "red" -> return (Red num)
        "green" -> return (Green num)
        "blue" -> return (Blue num)
    return ((== Just GT) `any` (compareCube cube `map` criteria))

parsPossible :: ReadP (Int, [Bool])
parsPossible = do
    skipSpaces
    id <- parsHead
    possible <- parsPossibleCube `sepBy` parsSep
    return (id, possible)

readPossibleLine :: String -> (Int, [Bool])
readPossibleLine s = fst $ last $ readP_to_S parsPossible s

isPossible :: String -> Int
isPossible = undefined
--isPossible inputLine = fst $ last $ readP_to_S parsPossible inputLine

sumPossible input = sum $ map isPossible (lines input)

main :: IO ()
main = do
    input <- readFile inputfile
    putStrLn $ show $ sumPossible input
