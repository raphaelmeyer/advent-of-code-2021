module Day09.Grid where

data Cursor a = Cursor {getRow :: ([a], [a]), getUp :: [a], getDown :: [a]} | Invalid deriving (Show, Eq)

data Grid a = Grid {getCursor :: Cursor a, getRows :: [[a]]} | Done deriving (Show, Eq)

data Neighbors a = Neighbors {nbLeft :: Maybe a, nbRight :: Maybe a, nbUp :: Maybe a, nbDown :: Maybe a} deriving (Show, Eq)

fromList :: [[a]] -> Grid a
fromList (row : down : rest) = Grid {getCursor = Cursor {getRow = ([], row), getUp = [], getDown = down}, getRows = row : down : rest}
fromList [row] = Grid {getCursor = Cursor {getRow = ([], row), getUp = [], getDown = []}, getRows = [row]}
fromList _ = Done

advance :: Grid a -> Grid a
advance Grid {getCursor = Cursor {getRow = (_, [_])}, getRows = rows} = nextRow rows
advance Grid {getCursor = cursor, getRows = rows} = Grid {getCursor = advance' cursor, getRows = rows}
advance _ = Done

advance' :: Cursor a -> Cursor a
advance' Cursor {getRow = (_, left : right), getUp = [], getDown = []} = Cursor {getRow = ([left], right), getUp = [], getDown = []}
advance' Cursor {getRow = (_, left : right), getUp = [], getDown = (_ : down)} = Cursor {getRow = ([left], right), getUp = [], getDown = down}
advance' Cursor {getRow = (_, left : right), getUp = (_ : up), getDown = []} = Cursor {getRow = ([left], right), getUp = up, getDown = []}
advance' Cursor {getRow = (_, left : right), getUp = (_ : up), getDown = (_ : down)} = Cursor {getRow = ([left], right), getUp = up, getDown = down}
advance' _ = Invalid

nextRow :: [[a]] -> Grid a
nextRow [row, next] = Grid {getCursor = Cursor {getRow = ([], next), getUp = row, getDown = []}, getRows = []}
nextRow (row : next : rows) = Grid {getCursor = Cursor {getRow = ([], next), getUp = row, getDown = head rows}, getRows = next : rows}
nextRow _ = Done

foldWithNeighbors :: (b -> (a, Neighbors a) -> b) -> b -> Grid a -> b
foldWithNeighbors _ acc Done = acc
foldWithNeighbors f acc grid = f acc' (value, neighbors)
  where
    acc' = foldWithNeighbors f acc (advance grid)
    value = getValue grid
    neighbors = getNeighbors grid

getValue :: Grid a -> a
getValue Grid {getCursor = Cursor {getRow = (_, value : _)}} = value
getValue _ = error "No value"

getNeighbors :: Grid a -> Neighbors a
getNeighbors Grid {getCursor = cursor} = Neighbors {nbLeft = neighborLeft cursor, nbRight = neighborRight cursor, nbUp = neighborUp cursor, nbDown = neighborDown cursor}
getNeighbors _ = Neighbors {nbLeft = Nothing, nbRight = Nothing, nbUp = Nothing, nbDown = Nothing}

neighborLeft :: Cursor a -> Maybe a
neighborLeft Cursor {getRow = ([left], _)} = Just left
neighborLeft _ = Nothing

neighborRight :: Cursor a -> Maybe a
neighborRight Cursor {getRow = (_, _ : right : _)} = Just right
neighborRight _ = Nothing

neighborUp :: Cursor a -> Maybe a
neighborUp Cursor {getUp = up : _} = Just up
neighborUp _ = Nothing

neighborDown :: Cursor a -> Maybe a
neighborDown Cursor {getDown = down : _} = Just down
neighborDown _ = Nothing
