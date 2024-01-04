module Main where

import Schema
import Data.List (nubBy, sort)
import Data.Char (isDigit)

inputfile = "input_3.txt"

-- 1st star: load the input file into the schematic
loadEngine :: Size -> String -> Schema
loadEngine size input = readSchema size input

-- Find all non-digit values in the schema
searchSymbols :: Schema -> [Char] -> [Cell]
searchSymbols sc valid = filter (\cell ->  (cellValue cell) `elem` valid) $ cells sc

-- and get  the adjacent digits, precisely the coordinates of the digits
-- ToDo: remove this function and use the AdjacentCell_to_Cell instead
getAdjacentCoords_to_Cells :: Schema -> [Cell] -> [Coord]
getAdjacentCoords_to_Cells sc cells = nubBy (\(i,j) (x,y) -> i==x && j==y) $ concat $ map adjCells selected where
    selected = map (\((i,j),c) -> (i,j)) $ hCells_to_Coords cells

getAdjacentCells_to_Cells :: Schema -> [Cell] -> [Cell]
getAdjacentCells_to_Cells sc cells = getCells sc coords where
    coords = nubBy (\(i,j) (x,y) -> i==x && j==y) $ concat $ map adjCells selected
    selected = map (\((i,j),c) -> (i,j)) $ hCells_to_Coords cells

getAdjacentCells_to_Cell :: Schema -> Cell -> [Cell]
getAdjacentCells_to_Cell sc cell = getCells sc coords where
    coords = adjCells selected
    selected = fst $ hCell_to_Coord cell

-- after erase all the symbols
eraseSymbols :: Schema -> Schema
eraseSymbols sc = Schema (size sc) newcells where
    newcells = sort $ readCells (size sc) ss
    ss = map (\c -> if c `elem` validSymbol then '.' else c) (showCells $ cells sc)


--  let parsedLS = filter (not . all (==True) . map isDigit) $ parseCells (cells erased)
--  let resultLS = map (map (\c -> if isDigit c then c else translateInv c)) $ parsedLS
--  let resultInt = map read resultLS


-- 2nd star
--

main :: IO ()
main = do
--    input <- readFile inputfile
--    putStrLn $ "Result for 1st star: "
--    let engine = loadEngine (Size 140 140) input
--    let symbols = searchSymbols engine validSymbol
--    let adjacent = getAdjacentCoords_to_Cells engine symbols
--    let marked = mapCells engine adjacent translate -- and mark the digits adjacent the symbols (0 -> A, ...)
--    let erased = eraseSymbols marked
-- | and parse the remaining cells as integers, but translate back the marked digits
--    let parsedLS = filter (not . all (==True) . map isDigit) $ parseCells (cells erased)
--    let resultLS = map (map (\c -> if isDigit c then c else translateInv c)) $ parsedLS
--    let resultInt = map read resultLS
--    putStrLn $ show $ sum resultInt

    putStrLn $ "Result for 2nd star: "
    let test_engine = loadEngine (Size 10 10 ) test_2         -- load test input
    putStrLn $ show $ test_engine
    let gears = searchSymbols test_engine "*"                 -- search for gears ("*")
    putStrLn $ show $ gears
    let adjacentCe = map (getAdjacentCells_to_Cell test_engine) gears  -- [[Cell]]
    putStrLn $ show $ adjacentCe
    putStrLn $ show $ findNonAdjDigits 0 <$> showCells <$> adjacentCe
    putStrLn "end program"


test_2 = "\
\467..114..\
\...*......\
\..35..633.\
\......#...\
\617*......\
\.....+.58.\
\..592.....\
\......755.\
\...$.*....\
\.664.598..\
\ "


