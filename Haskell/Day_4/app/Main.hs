{-# LANGUAGE StrictData #-}
module Main where

import Data.List
import Data.Ord

inputfile :: String
inputfile = "input_4.txt"

data Card = MkCard {
  card_id :: Int,
  winning :: [Int],
  we_have :: [Int]
  }

instance Show Card where
    show = show . card_id

instance Eq Card where
    card1 == card2 = card_id card1 == card_id card2

instance Ord Card where
    compare = comparing card_id

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

cardPoint' :: Card -> Int
cardPoint' card =
   foldr go 0 $ we_have card
    where
      go :: Int -> Int -> Int
      go i point
        | i `elem` winning card = point + 1
        | otherwise =  point

data Game =
   MkGame {
     draw_      :: Int
   , pile_      :: [Card]
   , my_pile_   :: [Card]
   , last_draw  :: Bool
    }

instance Show Game where
    show = show . draw_ <> const " - " <> const " - " <> show . my_pile_ <> const "\n"

makeDraw :: Game -> Game
makeDraw game =
    MkGame {
        draw_     = d0 + 1
      , pile_     = p0
      , my_pile_  = copy_prizes (next_card_id) (cardPoint' next_card)
      , last_draw = next_card_id == 0
    }
        where
            d0 = draw_ game
            p0 = pile_ game
            m0 = my_pile_ game

            next_card     = maybe cempty id $ m0!?d0
            next_card_id  = card_id next_card

            copy_prizes :: Int -> Int -> [Card] -- copyPrizes game i n copy n cards from the ith position of p pile into our pile
            copy_prizes i n = sort ((take n $ drop i $ p0) ++ m0)

main :: IO ()
main = do
    putStr "Test_1 result: "
    putStrLn $ show $ sum $ cardPoint <$> cardRead <$> lines test_1
    putStr "Result of Day 4 part 1: "
    input <- readFile inputfile
    putStrLn $ show $ sum $ cardPoint <$> cardRead <$> lines input

    putStr "Test_2 result: "
    let pile = cardRead <$> lines test_1
        game0 = MkGame 0 pile pile False
        last_game = until (\g -> last_draw g) makeDraw game0
     in putStrLn $ show $ length $ my_pile_ last_game

    putStr "Result of Day 4 star 2: "
    let pile = cardRead <$> lines input
        game0 = MkGame 0 pile pile False
        last_game = until (\g -> last_draw g) makeDraw game0
     in putStrLn $ show $ length $ my_pile_ last_game

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

cempty :: Card
cempty = MkCard { card_id = 0, winning = [], we_have = [] }

test_input :: IO [Card]
test_input = do
    input <- readFile inputfile
    pure (cardRead <$> lines input)
