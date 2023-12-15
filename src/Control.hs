{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Control where

import Board
-- import UI
import Brick
import Client
import qualified Data.Map as M
import Data.List (isInfixOf, transpose)
import Data.String ()
import Data.Char
import Data.Universe.Helpers
import Lens.Micro (ix, (%~), (^?), (^.))
import Lens.Micro.TH (makeLenses)
import Data.List.Split.Internals
import qualified Graphics.Vty as V
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)


data Ctrlc = Upc | Downc | Leftc | Rightc deriving (Show)
data Tick = Tick



-- Function to convert Blockstat to a character
blockstatToChar :: Blockstat -> Char
blockstatToChar (Occupied Black) = 'B'
blockstatToChar (Occupied White) = 'W'
blockstatToChar Empty = '-'

-- Function to convert a row of Blockstats to a string
rowToString :: [Blockstat] -> String
rowToString = map blockstatToChar

-- Function to convert the entire Board to a list of strings
boardToStrings :: Board -> [String]
boardToStrings (Mkboard board size) =
  [ rowToString [ board M.! (row, col) | col <- [0..size-1] ] | row <- [0..size-1] ]

-- Type alias for a row in the matrix
type Row = String

-- Type alias for the matrix
type Matrix = [Row]



containsW :: Matrix -> Int -> Bool
containsW matrix n = any (\row -> pattern `isInfixOf` row) matrix 
          where pattern = replicate n 'W'

checkWinW :: Matrix -> Int -> Bool
checkWinW matrix n = containsW matrix n || containsW (transpose matrix) n || containsW (diagonals matrix) n|| containsW (diagonals $ reverse matrix) n

containsB:: Matrix -> Int -> Bool
containsB matrix n = any (\row -> pattern `isInfixOf` row) matrix 
          where pattern = replicate n 'B'

checkWinB :: Matrix -> Int -> Bool
checkWinB matrix n = containsB matrix n || containsB (transpose matrix) n || containsB (diagonals matrix) n|| containsB (diagonals $ reverse matrix) n

checkWin :: Board -> Int -> (Bool,String) 
checkWin board n = if checkWinW (boardToStrings board) n then (True,"White wins")
                   else if checkWinB (boardToStrings board) n then (True, "Black wins")
                   else (False, "no one wins")

continNew :: GameState -> EventM () GameState ()
continNew game = do 
  put game
  return ()
  -- modify (\s -> game)

handleEvent :: BrickEvent () Tick -> EventM () GameState ()
handleEvent (VtyEvent (V.EvKey key [])) = do
  game <- get
  case key of
    -- Move cursor
    V.KUp       -> continNew $ cursorCtrl Upc game
    V.KDown     -> continNew $ cursorCtrl Downc game
    V.KLeft     -> continNew $ cursorCtrl Leftc game
    V.KRight    -> continNew $ cursorCtrl Rightc game
    V.KEnter -> addMoveToGame game

    -- V.KChar ' ' -> continNew $ if (_end game) 
    --                          then game 
    --                          else makeMove game
    V.KEsc -> halt
    _           -> continNew game

handleEvent (AppEvent Tick) = do
  game <- get
  if (_skt game) == Nothing then do return ()
    else if (_player game) == (_slf game) then do return ()
    else do
      let Just sock = _skt game
      estone <- liftIO (receiver sock)
      if estone==Nothing then do return ()
      else do
        let Just (m,n) = estone
        let boardSize = 15
        let getMap (Mkboard bd sz) = bd
        if (outOfBoard m n 15) then do return ()
         else if isOccupied (getMap (_board game)) m n then do return ()
          else do
            let updatedBoard = Mkboard (M.insert (m, n) (Occupied (_player game))  (getMap (_board game))) boardSize
            let game_res = checkWin updatedBoard (_rule game)
            continNew $ (game {_board = updatedBoard, _player = switchP game, _winner = checkWinAndUpdate game_res,_end = fst game_res})


handleEvent _ = return ()

-- handleEvent game (AppEvent Tick)  = continNew $ step game




cursorCtrl :: Ctrlc -> GameState -> GameState
cursorCtrl control game = (\c -> game { _cursor = c }) 
    $ case control of
    Leftc -> (x, bound(y - 1))
    Rightc -> (x, bound (y + 1))
    Downc  -> (bound (x + 1), y)
    Upc  -> (bound (x - 1), y)
  where
    (x, y) = _cursor game
    bound n
      | n >= 16    = n - 16                   ---changes this to the size of the board
      | n < 0     = n + 16
      | otherwise = n

switchP :: GameState -> Side
switchP game = if (_player game) == White then Black else White

--need to write a function that is able to transform cursor to the board

makeMove :: GameState -> GameState
makeMove game = if fst (checkWin (_board game) (_rule game)) 
           then game {_end = True}
           else game {_cursor = (7, 8), _player = (switchP game)}               --not finished should be able to call addMove from Board.hs
                                                                              --Note: here player would not switch if the previous move wins the game. Thus, can extract the winner as the current player 


--Try adding this new function but not tested
addMoveToGame :: GameState -> EventM () GameState ()
addMoveToGame game=
    if _end game
        then continNew game  -- If the game has already ended, do nothing
        else if ((_player game) /= (_slf game)) && ((_skt game) /= Nothing)
          then continNew game
          else if outOfBoard m n 15
              then continNew game -- If the move is out of bounds, do nothing
              else if isOccupied (getMap (_board game)) m n 
                  then continNew game -- If the cell is already occupied, do nothing
                  else if ((_player game) == (_slf game)) && ((_skt game) /= Nothing)
                    then do 
                      let stres = (show m) ++ " " ++ (show n)
                      let Just sock = (_skt game)
                      res <- liftIO (sender sock stres)
                      let game_res = checkWin updatedBoard (_rule game)
                      continNew $ game {_board = updatedBoard, _player = switchP game, _winner = checkWinAndUpdate game_res,_end = fst game_res}

                    else do
                      let game_res = checkWin updatedBoard (_rule game)
                      continNew $ game {_board = updatedBoard, _player = switchP game, _winner = checkWinAndUpdate game_res,_end = fst game_res}
  where
    boardSize = 15
    updatedBoard = Mkboard (M.insert (m, n) (Occupied (_player game))  (getMap (_board game))) boardSize            --include the boardSize in 
    (m,n) = _cursor game
    getMap (Mkboard bd sz) = bd


-- This is going to take use the checkWin to update the state
checkWinAndUpdate :: (Bool, String) -> Blockstat
checkWinAndUpdate (True, "White wins") = Occupied White
checkWinAndUpdate (True, "Black wins") = Occupied Black
checkWinAndUpdate _                    = Empty


-- controlMain :: IO ()
-- controlMain = do
  
--   let exampleBoard = initBoard sampleinit 15
--       new =boardToStrings exampleBoard
--       g = Game exampleBoard 5 (Occupied White) White Black (2,3) True Nothing
  
--   simpleMain (drawUI g)
--   putStr $ showBoard exampleBoard
--   print $ boardToStrings exampleBoard
--   print (diagonals $ reverse new)
--   print (diagonals new)
--   print (checkWinB new 3)
--   print (checkWin exampleBoard 5)
--   let g2 = g { _rule=6 }
--   print (_rule g2)