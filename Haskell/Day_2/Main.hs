-- Advent of code 2023 Day 1
-- main program get input data from inputfile file

import Data.Char (isDigit)
import Data.List (groupBy)
import qualified Data.Map as Map
import ParsCube
import Text.ParserCombinators.ReadP

inputfile = "input_2.txt"

-- 1st star
criteria = [Red 12, Green 13, Blue 14]

-- ParsCube modul parses Cube type, readLine parses input line to [Cube]
-- but now we need only the line number for which the games are possible

parsPossibleCube :: ReadP (Bool)
parsPossibleCube = do
    num <- fmap read $ munch1 isDigit
    optional (char ' ')
    color <- string "red" +++ string "green" +++ string "blue"
    cube <- case color of
        "red" -> return (Red num)
        "green" -> return (Green num)
        "blue" -> return (Blue num)
    return (not $ (== Just GT) `any` (compareCube cube `map` criteria))

parsPossible :: ReadP (Int, [Bool])
parsPossible = do
    skipSpaces
    id <- parsHead
    possible <- parsPossibleCube `sepBy` parsSep
    return (id, possible)

readPossibleLine :: String -> (Int, [Bool])
readPossibleLine s = fst $ last $ readP_to_S parsPossible s

isPossible :: String -> Int
isPossible inputLine
    | any (False ==) $ snd result  = 0
    | otherwise = fst result
    where result = readPossibleLine inputLine

sumPossible input = sum $ map isPossible (lines input)

-- 2nd star

parsPower :: ReadP (Int, [Cube])
parsPower = do
    skipSpaces
    id <- parsHead
    listCube <- parsCube `sepBy` parsSep
    let tupleset = map tupleToCube $ Map.toList $ Map.fromListWith max $ map cubeToTuple listCube
    return (id, tupleset)

readPower :: String -> [Cube]
readPower s = snd $ fst $ last $ readP_to_S parsPower s

powerOfLine :: String -> Int
powerOfLine inputLine = product $ map (snd . cubeToTuple) $ readPower inputLine

sumPower input = sum $ map powerOfLine (lines input)

main :: IO ()
main = do
    input <- readFile inputfile
    putStrLn $ "Result for 1st star: " ++ show (sumPossible input)
    putStrLn $ "Result for 2nd star: " ++ show (sumPower input)
