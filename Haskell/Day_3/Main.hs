module Main where

import Schema

inputfile = "input_3.txt"

-- 1st star
-- First find all non-digit values in the schema and mark the adjacent digits (add ord 'A' tho them)
loadEngine :: String -> Schema
loadEngine input = readSchema (Size 140 140) input

searchSymbols :: Schema -> [Char] -> [Cell]
searchSymbols sc valid = filter (\cell ->  (cellValue cell) `elem` valid) $ cells sc

markDigitsAdjacent :: Schema -> [Cell] -> Schema
markDigitsAdjacent sc = undefined


-- set the value of many cells
-- foldl (\sc ((a,b), c) -> setCell sc (a,b) c) puzzle [((2,1),'5'), ((2,2), '8')]

eraseSymbols :: Schema -> Schema
eraseSymbols sc = Schema (size sc) newcells where
    newcells = readCells (size sc) ss
    ss = map (\c -> if c `elem` validSymbol then '.' else c) (showCells $ cells sc)


-- 2nd star
--
main :: IO ()
main = do
    input <- readFile inputfile
    putStrLn $ "Result for 1st star: "
    let engine = loadEngine input 
    let marked = markDigitsAdjacent engine
    let symbols = searchSymbols engine validSymbol
    putStrLn $ show $ take 3 symbols
 --   let erased = eraseSymbols engine
 --   putStrLn $ show $ erased
--    putStrLn $ "Result for 2nd star: " ++ show (input)
    putStrLn $ show $ puzzle
