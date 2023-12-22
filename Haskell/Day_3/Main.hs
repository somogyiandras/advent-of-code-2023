module Main where

import Schema

inputfile = "input_3.txt"



-- 1st star


-- 2nd star
--
main :: IO ()
main = do
    input <- readFile inputfile
    putStrLn $ "Result for 1st star: " ++ show (input)
    putStrLn $ "Result for 2nd star: " ++ show (input)
