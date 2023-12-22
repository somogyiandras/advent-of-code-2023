module Schema where

import Data.List (unfoldr, sort)

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

data Schema = Schema {
    size :: Size,     
    cells :: [Cell]
    }

instance Show Schema where
    show sc = map cellValue (sort $ cells sc)

initSchema :: Size -> Schema
initSchema (Size m n) =
     Schema { size = (Size m n),
              cells = genCells (Size 140 140) '.'
            }

genCells :: Size -> Char -> [Cell]
genCells (Size m n) c =
    unfoldr hGenCells (1, 1, m, n, c)

hGenCells :: (Int, Int, Int, Int, Char) -> Maybe (Cell, (Int, Int, Int, Int, Char))
hGenCells (i, j, m, n, c)
  | i > m || j > n || i < 0 || j < 0  = Nothing
  | i <= m && j < n = Just (Cell (i, j) c, (i, j+1, m, n, c))
  | j == n && i /= m = Just (Cell (i, j) c, (i+1, 1, m, n, c))
  | i == m && j == n = Just (Cell (i, j) c, (m+1, n+1, m, n, c))

validCell = "#%&*+-./=@$0123456789"

isValid :: Char -> Bool
isValid = (`elem` validCell)

-- | test data
puzzle = Schema { size = Size 3 3,
    cells = [Cell (1,1) '.', Cell (1,2) '*', Cell (1, 3) '.',
             Cell (2,1) '.', Cell (2,2) '*', Cell (2, 3) '.',
             Cell (3,1) '.', Cell (3,2) '*', Cell (3, 3) '.'] }

sc = initSchema (Size 140 140)
