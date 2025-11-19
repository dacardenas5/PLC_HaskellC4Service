-- main module for connect four
-- handles io and game flow
-- uses board module for all logic

module Main where

import Board
import System.IO (hFlush, stdout)

-- map player to char for display
playerToChar :: Int -> Char
playerToChar 1 = 'O'
playerToChar 2 = 'X'
playerToChar _ = '?' -- fallback for unknown

-- read a valid slot from user
-- keeps asking until input is ok
readSlot :: Board -> Int -> IO Int
readSlot bd p = do
  putStr $ "player " ++ show p ++ " (" ++ [playerToChar p] ++ "), choose a slot (1-" ++ show (numSlot bd) ++ "): "
  hFlush stdout
  line <- getLine
  case reads line :: [(Int, String)] of
    [] -> do
      putStrLn "not a number, try again"
      readSlot bd p
    ((x, _) : _) ->
      if x < 1 || x > numSlot bd
        then do
          putStrLn $ "invalid slot, pick 1 to " ++ show (numSlot bd)
          readSlot bd p
        else
          if not (isSlotOpen bd x)
            then do
              putStrLn "slot full, pick another"
              readSlot bd p
            else return x

-- print board state
printBoard :: Board -> IO ()
printBoard bd = putStrLn (boardToStr playerToChar bd)

-- switch player
other :: Int -> Int
other 1 = 2
other 2 = 1
other x = if x == 1 then 2 else 1 -- fallback

-- main game loop
gameLoop :: Board -> Int -> IO ()
gameLoop bd p = do
  putStrLn "" -- blank line
  printBoard bd
  if isFull bd
    then putStrLn "draw! board full"
    else do
      slot <- readSlot bd p
      let bd' = dropInSlot bd slot p
      if isWonBy bd' p
        then do
          putStrLn ""
          printBoard bd'
          putStrLn $ "player " ++ show p ++ " (" ++ [playerToChar p] ++ ") wins!"
        else gameLoop bd' (other p)

-- entry point
-- starts 7x6 game
main :: IO ()
main = do
  putStrLn "connect four (7x6) - horiz & vert wrap"
  let bd = mkBoard 7 6
  gameLoop bd mkPlayer
