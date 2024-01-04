module Schema where

import Data.List -- (unfoldr, sort, intercalate, uncons, find)
import Data.Char
import Debug.Trace

-- | chunksOf from Data.List.Split
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
 where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n = l `c` splitter (drop i l) c n

validRead = "#%&*+-./=@$0123456789ABCDEFGHIJ"
validSymbol = "#%&*+-/=@$"

shift = ord 'A' - ord '0'

isValidRead :: Char -> Bool
isValidRead = (`elem` validRead)

translate :: Char -> Char
translate = chr . (shift +) . ord

translateInv :: Char -> Char
translateInv c = chr $ ord c - shift

type Coord = (Int, Int)

data Size = Size Int Int
    deriving (Eq, Show)

data Cell = Cell (Int, Int) Char
    deriving (Show)

instance Eq Cell where
    (==) (Cell (i1, j1) _) (Cell (i2, j2) _) = i1 == i2 && j1 == j2

instance Ord Cell where
    compare (Cell (i1, j1) _) (Cell (i2, j2) _)
      | i1 == i2 = j1 `compare` j2
      | i1 < i2 = LT
      | i1 > i2 = GT

cellValue :: Cell -> Char
cellValue (Cell (_, _) c) = c

showCells :: [Cell] -> String
showCells = map cellValue . sort

showCells' :: Int -> [Cell] -> String
showCells' width cs = intercalate "\n" $ chunksOf width $ showCells cs 

-- | parseCells "read" the numbers from the string
parseCells :: [Cell] -> [String]
parseCells = words . map (\c -> if c=='.' then ' ' else c) . showCells

initCells :: Size -> Char -> [Cell]
initCells (Size m n) c = [Cell (i, j) c | i <- [1..m], j <- [1..n]]

readCells :: Size -> String -> [Cell]
readCells (Size m n) s = zipWith (\coord c -> Cell coord c) coords ss where
    coords = [(i, j) | i <- [1..m], j <- [1..n]]
    ss = [ c | c <- s, isValidRead c]

adjCells :: Coord -> [Coord]
adjCells (i, j) = [ (x, y) | x <- [i-1 .. i+1], y <- [j-1 .. j+1], (x, y) /= (i, j) ]

hCell_to_Coord :: Cell -> (Coord, Char)
hCell_to_Coord (Cell (i,j) c) = ((i,j),c)

hCells_to_Coords :: [Cell] -> [(Coord, Char)]
hCells_to_Coords = map hCell_to_Coord

data Schema = Schema {
    size :: Size,     
    cells :: [Cell]
    }

instance Show Schema where
    show Schema { size = Size height width, cells = cs } = intercalate "\n" $ chunksOf width $ showCells cs

initSchema :: Size -> Schema
initSchema (Size m n) =
     Schema { size = (Size m n),
              cells = initCells (Size m n) '.' }

readSchema :: Size -> String -> Schema
readSchema (Size m n) input =
    Schema { size = (Size m n),
             cells = readCells (Size m n) input }

getCellChar :: Schema -> Coord -> Char
getCellChar sc (i, j) = case find (== Cell (i, j) ' ') $ cells sc of
                          Nothing -> ' '
                          Just (Cell (_, _) c) -> c

getCell :: Schema -> Coord -> Cell
getCell sc (i, j) = case find (== Cell (i, j) ' ') $ cells sc of
                      Nothing -> Cell (0, 0) ' '
                      Just cell -> cell

getCells :: Schema -> [Coord] -> [Cell]
getCells sc coords = filter (/= Cell (0, 0) ' ') $ map (getCell sc) coords

setCell :: Schema -> Coord -> Char -> Schema
setCell sc (i, j) c = Schema (size sc) (newcells) where
    newcells = Cell (i, j) c : delete (Cell (i, j) ' ') (cells sc)

mapCells :: Schema -> [Coord] -> (Char -> Char) -> Schema
mapCells sc coords tr = foldl (\sc' ((a,b), c) -> setCell sc' (a,b) c) sc translated  where
    cells = hCells_to_Coords $ getCells sc coords
    translated = map (\((i, j), c) ->
       if isDigit c
          then ((i, j) , tr c)
          else ((i, j) , c)
      )
      cells

findNonAdjDigits :: Int -> String -> Int
findNonAdjDigits n [] = n
findNonAdjDigits n (c1:c2:cs) = if isDigit c1
                                   then if isDigit c2
                                          then findNonAdjDigits n (c2:cs)
                                          else findNonAdjDigits (n+1) cs
                                   else findNonAdjDigits n cs
findNonAdjDigits n (c:[]) = if isDigit c
                              then findNonAdjDigits (n+1) []
                              else findNonAdjDigits n []

-- | test data
puzzle = Schema { size = Size 3 3,
    cells = [Cell (1,1) '1', Cell (1,2) '*', Cell (1, 3) '$',
             Cell (2,1) '2', Cell (2,2) '*', Cell (2, 3) 'E',
             Cell (3,1) 'A', Cell (3,2) '*', Cell (3, 3) 'F'] }

sc =  (Size 140 140)

