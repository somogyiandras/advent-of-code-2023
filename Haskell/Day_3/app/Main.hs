module Main where

import Schema
import Data.List (nub, nubBy, sort)
import Data.Char (isDigit)

inputfile :: String
inputfile = "input_3.txt"

------------------------------------------------------------
-- 1st star

loadEngine :: Size -> String -> Schema                              -- load the input file into the schematic
loadEngine nm input = readSchema nm input

searchSymbols :: Schema -> [Char] -> [Cell]                         -- Find all non-digit values in the schema
searchSymbols sc valid = filter (\cell ->  (cellValue cell) `elem` valid) $ cells sc


-- ToDo: remove this function and use the AdjacentCell_to_Cell instead, or use only the coords
-- using the cells (with values) is more general, but maybe unnessesary
getAdjacentCoords_to_Cells :: [Cell] -> [Coord]                     -- and get  the adjacent digits, precisely the coordinates of the digits
getAdjacentCoords_to_Cells coords = nubBy (\(i,j) (x,y) -> i==x && j==y) $ concat $ map adjCells selected where
    selected = map (\((i,j),_) -> (i,j)) $ hCells_to_Coords coords

getAdjacentCells_to_Cells :: Schema -> [Cell] -> [Cell]
getAdjacentCells_to_Cells sc ces = getCells sc coords where
    coords = nubBy (\(i,j) (x,y) -> i==x && j==y) $ concat $ map adjCells selected
    selected = map (\((i,j),_) -> (i,j)) $ hCells_to_Coords ces

getAdjacentCells_to_Cell :: Schema -> Cell -> [Cell]
getAdjacentCells_to_Cell sc cell = getCells sc coords where
    coords = adjCells selected
    selected = fst $ hCell_to_Coord cell


eraseSymbols :: Schema -> Schema                                      -- after that erase all the symbols
eraseSymbols sc = Schema (size sc) newcells where                     -- the other calculations in the main
    newcells = sort $ readCells (size sc) ss
    ss = map (\c -> if c `elem` validSymbol then '.' else c) (showCells $ cells sc)

--------------------------------------------------------------------
-- 2nd star

getContextOfGear :: Schema -> Cell -> [Int]
getContextOfGear sc cell = nub $ foldl h_get_int [] $ getAdjacentCells_to_Cell sc cell
  where
      h_get_int :: [Int] -> Cell -> [Int]
      h_get_int results cell_ = case getIntAtCell sc cell_ of
                                  Nothing -> results
                                  Just int -> int : results


main :: IO ()
main = do
    input <- readFile inputfile
    putStrLn $ "Result for 1st star: "
    let engine = loadEngine (Size 140 140) input
    let symbols = searchSymbols engine validSymbol
    let adjacent = getAdjacentCoords_to_Cells symbols
    let marked = mapCells engine adjacent translate -- and mark the digits adjacent the symbols (0 -> A, ...)
    let erased = eraseSymbols marked
-- | and parse the remaining cells as integers, but translate back the marked digits
    let parsedLS = filter (not . all (==True) . map isDigit) $ parseCells (cells erased)
    let resultLS = map (map (\c -> if isDigit c then c else translateInv c)) $ parsedLS
    let resultInt = map read resultLS :: [Int]
    putStrLn $ show $ sum resultInt

    putStrLn $ "Result for 2nd star: "
    let gears = searchSymbols engine "*"                 -- search for gears ("*")
    let ratios = getContextOfGear engine <$> gears
    let ratio = filter (\l -> length l == 2) ratios
-- This approach is no good, it can happen that two same integers appear next the '*'
-- Todo: change the code to handle this situation
    let result2 = foldl (\acc gs -> acc + head gs * last gs) 0 ratio
    putStrLn $ show result2
    putStrLn "end test"
