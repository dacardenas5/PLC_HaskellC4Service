-- main module for connect four
-- handles io and game flow
-- uses board module for all logic

module Main where

import Board
import Data.Time.Clock.POSIX (getPOSIXTime)
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

-- computer chooses a random open slot
getRandomSlot :: Board -> IO Int
getRandomSlot bd = do
  t <- getPOSIXTime
  let seed = (floor (t * 1000)) `mod` numSlot bd + 1
  if isSlotOpen bd seed
    then return seed
    else getRandomSlot bd

-- print board state
printBoard :: Board -> IO ()
printBoard bd = putStrLn (boardToStr playerToChar bd)

-- switch player
other :: Int -> Int
other 1 = 2
other 2 = 1
other x = if x == 1 then 2 else 1 -- fallback

-- human vs human game loop
gameLoopHuman :: Board -> Int -> IO ()
gameLoopHuman bd p = do
  putStrLn ""
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
        else gameLoopHuman bd' (other p)

-- human vs computer game loop (player 2 is computer)
gameLoopComputer :: Board -> Int -> IO ()
gameLoopComputer bd p = do
  putStrLn ""
  printBoard bd
  if isFull bd
    then putStrLn "draw! board full"
    else do
      slot <-
        if p == 1
          then readSlot bd p
          else do
            putStrLn "computer (player 2) is choosing..."
            s <- getRandomSlot bd
            putStrLn $ "computer picked slot " ++ show s
            return s

      let bd' = dropInSlot bd slot p
      if isWonBy bd' p
        then do
          putStrLn ""
          printBoard bd'
          putStrLn $ "player " ++ show p ++ " (" ++ [playerToChar p] ++ ") wins!"
        else gameLoopComputer bd' (other p)

-- entry point with mode selection
main :: IO ()
main = do
  putStrLn "connect four (7x6) - horiz & vert wrap"
  putStrLn "1. play vs human"
  putStrLn "2. play vs computer"
  putStr "choose mode: "
  hFlush stdout
  mode <- getLine

  let bd = mkBoard 7 6
  case mode of
    "1" -> gameLoopHuman bd mkPlayer
    "2" -> gameLoopComputer bd mkPlayer
    _ -> do
      putStrLn "invalid choice"
      main
