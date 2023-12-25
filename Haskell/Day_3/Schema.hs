module Schema where

import Data.List -- (unfoldr, sort, intercalate, uncons, find)

-- | chunksOf from Data.List.Split
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
 where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n = l `c` splitter (drop i l) c n

validRead = "#%&*+-./=@$0123456789"
validSymbol = "#%&*+-/=@$"
validNumber = "0123456789ABCDEFGHIJK"

isValidRead :: Char -> Bool
isValidRead = (`elem` validRead)

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

-- | parseCells "read" the numbers from the string
parseCells :: [Cell] -> [String]
parseCells = words . map (\c -> if c=='.' then ' ' else c) . showCells

initCells :: Size -> Char -> [Cell]
initCells (Size m n) c = [Cell (i, j) c | i <- [1..m], j <- [1..n]]
--    unfoldr hGenCells (1, 1, m, n, c)

-- hGenCells :: (Int, Int, Int, Int, Char) -> Maybe (Cell, (Int, Int, Int, Int, Char))
-- hGenCells (i, j, m, n, c)
--   | i > m || j > n || i < 0 || j < 0  = Nothing
--   | i <= m && j < n = Just (Cell (i, j) c, (i, j+1, m, n, c))
--   | j == n && i /= m = Just (Cell (i, j) c, (i+1, 1, m, n, c))
--   | i == m && j == n = Just (Cell (i, j) c, (m+1, n+1, m, n, c))

readCells :: Size -> String -> [Cell]
readCells (Size m n) s = zipWith (\coord c -> Cell coord c) coords ss where
    coords = [(i, j) | i <- [1..m], j <- [1..n]]
    ss = [ c | c <- s, isValidRead c]
--  unfoldr hReadCells (1, 1, m, n, s)

-- hReadCells :: (Int, Int, Int, Int, String) -> Maybe (Cell, (Int, Int, Int, Int, String))
-- hReadCells (i, j, m, n, s)
--   | i > m || j > n || i < 0 || j < 0  = Nothing
--   | i <= m && j < n = Just (Cell (i, j) c, (i, j+1, m, n, cs))
--   | j == n && i /= m = Just (Cell (i, j) c, (i+1, 1, m, n, cs))
--   | i == m && j == n = Just (Cell (i, j) c, (m+1, n+1, m, n, cs))
--   where
--       ss = filter isValidRead s
--       c = case uncons ss of
--             Nothing -> '.'
--             Just (h, t) -> h
--       cs = case uncons ss of
--             Nothing -> "."
--             Just (h, t) -> t

adjCells :: Coord -> [Coord]
adjCells (i, j) = [ (x, y) | x <- [i-1 .. i+1], y <- [j-1 .. j+1], (x, y) /= (i, j) ]

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
mapCells sc coords tr = Schema (size sc) (newcells) where
    cells = getCells sc coords
    translated = undefined
    newcells = foldl (\sc' ((a,b), c) -> setCell sc' (a,b) c) sc coords


-- | test data
puzzle = Schema { size = Size 3 3,
    cells = [Cell (1,1) '.', Cell (1,2) '*', Cell (1, 3) '.',
             Cell (2,1) '.', Cell (2,2) '*', Cell (2, 3) '.',
             Cell (3,1) '.', Cell (3,2) '*', Cell (3, 3) '.'] }

sc =  (Size 140 140)

