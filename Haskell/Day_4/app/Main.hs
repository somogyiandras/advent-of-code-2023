module Main where

import Data.List

inputfile :: String
inputfile = "input_4.txt"

data Card = MkCard {
  card_id :: Int,
  winning :: [Int],
  we_have :: [Int]
  } deriving (Show)

cardRead :: String -> Card
cardRead input =
    let he  = stripPrefix "Card" $ fst $ break (== ':') input
        (mid, las)  = break (== '|') input
    in
      MkCard
        { card_id = case he of
                      Nothing -> 0
                      Just i  -> read i :: Int
        , winning = read <$> (words $ drop 1 $ dropWhile (/= ':') mid) :: [Int]
        , we_have = read <$> (words $ drop 1 las) :: [Int] }

cardPoint :: Card -> Int
cardPoint card =
   foldr go 0 $ we_have card
    where
      go :: Int -> Int -> Int
      go i point
        | point == 0 && i `elem` winning card = 1
        | i `elem` winning card = 2*point
        | otherwise =  point


main :: IO ()
main = do
    putStr "Test_1 result: "
    putStrLn $ show $ sum $ cardPoint <$> cardRead <$> lines test_1
    putStr "Result of Day 4 part 1: "
    input <- readFile inputfile
    putStrLn $ show $ sum $ cardPoint <$> cardRead <$> lines input

---------------------------------
-- test data
--
test_1 :: String
test_1 = "\
  \Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
  \Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
  \Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
  \Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
  \Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
  \Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

c0 :: Card
c0 = MkCard { card_id = 0, winning = [41, 48, 83, 86, 17], we_have = [83, 86, 6, 31, 17, 9, 48, 53] }
