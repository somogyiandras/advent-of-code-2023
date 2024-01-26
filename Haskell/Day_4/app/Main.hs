{-# LANGUAGE StrictData #-}
module Main where

import Data.List
import Data.Ord
import qualified Data.Map.Strict as M

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
   , my_pile_   :: M.Map Card Int
    }

instance Show Game where
    show = show . draw_ <> const " - " <> const " - " <> show . my_pile_ <> const "\n"

makeDraw :: Game -> Game
makeDraw game =
    MkGame {
        draw_     = d0 + 1
      , pile_     = p0
      , my_pile_  = add_prizes next_card_n $ cardPoint' next_card
    }
        where
            d0 = draw_ game
            p0 = drop 1 $ pile_ game
            m0 = my_pile_ game

            (next_card, next_card_n) = maybe (cempty, 0) id $ (M.assocs m0)!?d0

            add_prizes :: Int -> Int -> M.Map Card Int -- add_prizes l n: add the first n cards from pile to my_pile l times
            add_prizes l n = add_cards (take n $ p0) l m0
               where
                   add_card :: Card -> Int -> M.Map Card Int -> M.Map Card Int
                   add_card c times mp = M.insertWith (+) c times mp

                   add_cards :: [Card] -> Int -> M.Map Card Int -> M.Map Card Int
                   add_cards cs times mp = foldl' (\pile c -> add_card c times pile) mp cs
                    

main :: IO ()
main = do
    putStr "Test_1 result: "
    putStrLn $ show $ sum $ cardPoint <$> cardRead <$> lines test_1
    putStr "Result of Day 4 part 1: "
    input <- readFile inputfile
    putStrLn $ show $ sum $ cardPoint <$> cardRead <$> lines input

    putStr "Test_2 result: "
    let pile    = cardRead <$> lines test_1
        myPile0 = M.fromList $ zip pile $ replicate (length pile) 1
        game0 = MkGame 0 pile myPile0
        last_game = until (\g -> null $ pile_ g) makeDraw game0
        len :: M.Map Card Int -> Int
        len p = M.foldl' (\acc card -> acc + card) 0 p
        in putStrLn $ show $ len $ my_pile_ last_game
     -- in putStrLn $ show $ take 30 $ iterate makeDraw game0


    putStr "Result of Day 4 star 2: "
    let pile    = cardRead <$> lines input
        myPile0 = M.fromList $ zip pile $ replicate (length pile) 1
        game0 = MkGame 0 pile myPile0
        last_game = until (\g -> null $ pile_ g) makeDraw game0
        len :: M.Map Card Int -> Int
        len p = M.foldl' (\acc card -> acc + card) 0 p
        in putStrLn $ show $ len $ my_pile_ last_game
--     let pile = cardRead <$> lines input
--         myPile0 = M.fromList $ zip pile $ replicate (length pile) 1
--         game0 = MkGame 0 pile myPile0 False
--         last_game = until (\g -> last_draw g) makeDraw game0
--      in putStrLn $ show $ length $ my_pile_ last_game

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
