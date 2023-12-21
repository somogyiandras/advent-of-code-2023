module ParsCube where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

data Cube = Red Int | Green Int | Blue Int 

compareCube :: Cube -> Cube -> Maybe Ordering
compareCube (Red x) (Red y) = Just (x `compare` y)
compareCube (Blue x) (Blue y) = Just (x `compare` y)
compareCube (Green x) (Green y) = Just (x `compare` y)
compareCube _ _ = Nothing

instance Show Cube where
    show (Red i) = show i ++ " red"
    show (Green i) = show i ++ " green"
    show (Blue i) = show i ++ " blue"
    
instance Read Cube where
    readsPrec _ = readP_to_S parsCube

sameColor :: Cube -> Cube -> Bool
sameColor (Red _) (Red _) = True
sameColor (Green _) (Green _) = True
sameColor (Blue _) (Blue _) = True
sameColor _ _ = False

cubeToTuple :: Cube -> (String, Int)
cubeToTuple (Red n) = ("Red", n)
cubeToTuple (Green n) = ("Green", n)
cubeToTuple (Blue n) = ("Blue", n)

tupleToCube :: (String, Int) -> Cube
tupleToCube (("Red", n)) = Red n
tupleToCube (("Green", n)) = Green n
tupleToCube (("Blue", n)) = Blue n


parsSep :: ReadP Char
parsSep = do
    skipSpaces
    sep <- char ',' <++ char ';'
    skipSpaces
    return sep

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
    listCube <- parsCube `sepBy` parsSep
    return (id, listCube)
    
readLine :: String -> (Int, [Cube])
-- readLine s = fst $ last $ readP_to_S parsLine s
readLine = fst . last . readP_to_S parsLine
