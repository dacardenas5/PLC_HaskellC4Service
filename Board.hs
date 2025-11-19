-- board module for connect four
-- handles board state, players, win detection

module Board
  ( Board,
    mkBoard,
    mkPlayer,
    mkOpponent,
    dropInSlot,
    isSlotOpen,
    numSlot,
    isFull,
    isWonBy,
    boardToStr,
  )
where

-- board type, each column is a list top to bottom
type Board = [[Int]]

-- create empty board m x n
mkBoard :: Int -> Int -> Board
mkBoard m n
  | m <= 0 || n <= 0 = error "mkboard: dims must be positive"
  | otherwise = replicate m (replicate n 0) -- fill columns with 0

-- first player
mkPlayer :: Int
mkPlayer = 1

-- second player
mkOpponent :: Int
mkOpponent = 2

-- number of columns
numSlot :: Board -> Int
numSlot bd = length bd

-- check if column index is valid
inRangeCol :: Board -> Int -> Bool
inRangeCol bd i = i >= 1 && i <= numSlot bd

-- true if slot has at least one empty cell
isSlotOpen :: Board -> Int -> Bool
isSlotOpen bd i
  | not (inRangeCol bd i) = False
  | otherwise = any (== 0) (bd !! (i - 1)) -- check column

-- helper: update nth element (1-based)
updateNth :: Int -> (a -> a) -> [a] -> [a]
updateNth _ _ [] = error "updatenth: index out of range"
updateNth 1 f (x : xs) = f x : xs
updateNth n f (x : xs) = x : updateNth (n - 1) f xs

-- drop disc for player p in column i
-- assumes slot is open
dropInSlot :: Board -> Int -> Int -> Board
dropInSlot bd i p
  | not (inRangeCol bd i) = error "dropinslot: col out of range"
  | otherwise =
      let col = bd !! (i - 1)
          n = length col
          -- find last empty row
          findPos idx
            | idx < 1 = error "dropinslot: no empty cell"
            | col !! (idx - 1) == 0 = idx
            | otherwise = findPos (idx - 1)
          pos = findPos n
          newCol = updateNth pos (const p) col -- place disc
       in updateNth i (const newCol) bd

-- true if board full
isFull :: Board -> Bool
isFull bd = not (any (any (== 0)) bd)

-- get value at cell, 1-based
-- 0 if out of bounds
getCell :: Board -> Int -> Int -> Int
getCell bd c r
  | not (inRangeCol bd c) = 0
  | r < 1 || r > length (bd !! (c - 1)) = 0
  | otherwise = (bd !! (c - 1)) !! (r - 1)

-- how many in a row to win
winLen :: Int
winLen = 4

-- check if player p won
-- horiz/vert wrap, diagonal no wrap
isWonBy :: Board -> Int -> Bool
isWonBy bd p
  | winLen <= 1 = False
  | numSlot bd < 1 = False
  | nRows < 1 = False
  | otherwise = any id [checkHorizontal, checkVertical, checkDiagDownRight, checkDiagUpRight]
  where
    m = numSlot bd
    nRows = if m == 0 then 0 else length (head bd)

    wrapCol x = ((x - 1) `mod` m) + 1
    wrapRow y = ((y - 1) `mod` nRows) + 1

    -- horiz win check
    checkHorizontal =
      or
        [ all (\k -> getCell bd (wrapCol (c + k)) r == p) [0 .. (winLen - 1)]
          | r <- [1 .. nRows],
            c <- [1 .. m]
        ]

    -- vert win check
    checkVertical =
      or
        [ all (\k -> getCell bd c (wrapRow (r + k)) == p) [0 .. (winLen - 1)]
          | c <- [1 .. m],
            r <- [1 .. nRows]
        ]

    -- diag \ no wrap
    checkDiagDownRight =
      or
        [ all (\k -> getCell bd (c + k) (r + k) == p) [0 .. (winLen - 1)]
          | c <- [1 .. m],
            r <- [1 .. nRows],
            c + (winLen - 1) <= m,
            r + (winLen - 1) <= nRows
        ]

    -- diag / no wrap
    checkDiagUpRight =
      or
        [ all (\k -> getCell bd (c + k) (r - k) == p) [0 .. (winLen - 1)]
          | c <- [1 .. m],
            r <- [1 .. nRows],
            c + (winLen - 1) <= m,
            r - (winLen - 1) >= 1
        ]

-- convert board to string
-- use playerToChar for players, '.' for empty
boardToStr :: (Int -> Char) -> Board -> String
boardToStr playerToChar bd
  | null bd = ""
  | otherwise =
      let m = numSlot bd
          n = length (head bd)
          -- get char for cell
          cellChar r c = let v = getCell bd c r in if v == 0 then '.' else playerToChar v
          rowStr r = unwords [[cellChar r c] | c <- [1 .. m]]
          rows = [rowStr r | r <- [1 .. n]]
       in unlinesNoTrailing rows

-- join lines without trailing newline
unlinesNoTrailing :: [String] -> String
unlinesNoTrailing [] = ""
unlinesNoTrailing [x] = x
unlinesNoTrailing (x : xs) = x ++ '\n' : unlinesNoTrailing xs
