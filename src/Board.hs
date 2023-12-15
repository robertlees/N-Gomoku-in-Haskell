{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Board where

import qualified Data.Map as M
import Data.List (isInfixOf, transpose)
import Data.String ()
import Data.Char
import Data.Universe.Helpers
import Lens.Micro (ix, (%~), (^?), (^.))
import Lens.Micro.TH (makeLenses)
import Data.List.Split.Internals
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)


data Side = Black
            | White
            deriving (Eq, Show)            

data Blockstat = Occupied Side
                | Empty 
                deriving (Eq, Show)

data Board = Mkboard (M.Map (Int, Int) Blockstat) Int  deriving(Show)

data GameState = Game
  { _board  :: Board -- ^ Gomoku Board
  , _rule   :: Int     -- ^ how many stones in a line will win the game?
  , _winner :: Blockstat  -- ^ winner of the game
  , _slf     :: Side -- ^ identity of self
  , _player :: Side -- ^ current player
  , _cursor  :: (Int, Int) -- ^ cursor's location
  , _end   :: Bool -- ^ is the game end?
  , _skt :: Maybe Socket -- ^ network socket
  } deriving (Show)

makeLenses ''GameState
-- $ (makeLenses ''GameState)
-- makeLenses ''GameState

data Ctrl = 
  Up | Down | Left | Right

-- moveCursor :: Dir -> Game -> Game
-- moveCursor direction game =
--   (\c -> game { cursor = c }) $ case direction of
--     Game.Up -> (x, wrap (y - 1))
--     Game.Down -> (x, wrap (y + 1))
--     Game.Right  -> (wrap (x + 1), y)
--     Game.Left  -> (wrap (x - 1), y)
--   where
--     (x, y) = cursor game
--     wrap n
--       | n >= 9    = n - 9
--       | n < 0     = n + 9
--       | otherwise = n



showBlock :: Blockstat -> String
showBlock b = case b of 
          Occupied White -> " o"
          Occupied Black -> " x"
          Empty -> " -" 

pickRow :: Board -> Int -> Int -> Blockstat
pickRow (Mkboard board size) m n = board M.! (m,n)


showRow :: Board -> Int -> String
showRow (Mkboard board size) m = show m ++ concat (map showBlock (map (pickRow (Mkboard board size) m) [0..(size-1)])) ++ " " ++ show m ++ "\n"

labelRow :: Int -> String
labelRow size = 
            if size==1 then 
            "  0"
            else if (size < 10 && size > 1) then 
                 labelRow (size-1) ++ " " ++ show (size-1)
            else 
                 labelRow (size-1) ++ " " ++ [chr (ord 'a' + (size-10))]

showBoard :: Board -> String
showBoard (Mkboard board size) =
    labelRow size ++ "\n" ++ concat (map (showRow (Mkboard board size)) [0..(size-1)]) ++ labelRow size


emptyBoard :: Int -> Board
emptyBoard n = Mkboard  (M.fromList [ ((i,j), Empty) | i <- [0..(n-1)], j <- [0..(n-1)] ])  n


initBoard :: [((Integer,Integer),  Blockstat)] -> Int -> Board
initBoard initlist n = Mkboard  (  M.fromList (initintmap initlist) `M.union` M.fromList [ ((i,j), Empty) | i <- [0..(n-1)], j <- [0..(n-1)] ])  n


initint :: (Integral a1, Integral a2, Num a3, Num b1) => ((a1, a2), b2) -> ((a3, b1), b2)
initint ((a, b), c ) = ((fromIntegral a, fromIntegral b ), c) 

initintmap :: (Integral a1, Integral a2, Num a3, Num b1) => [((a1, a2), b2)] -> [((a3, b1), b2)]

initintmap = map initint 




isOccupied :: (Ord a, Ord b) => M.Map (a, b) Blockstat -> a -> b -> Bool
isOccupied  board  m n  =   board M.! (m,n) /= Empty

-- isOccupiedBoard :: (Ord a, Ord b) => Board -> a -> b -> Bool
-- isOccupiedBoard (Mkboard board _) m n  =  board M.! (m,n) /= Empty


outOfBoard :: (Ord a, Num a) => a -> a -> a -> Bool
outOfBoard m n size = if m>=size then True
                     else if m<0 then True
                     else if n>=size then True
                     else if n<0 then True
                     else False

addMove :: Board -> Int -> Int -> Side -> (Board, Bool)
addMove (Mkboard board size) m n side =  if outOfBoard m n size  then (Mkboard board size, False)
                                        else  if isOccupied board m n then (Mkboard board size, False)
                                        else   (Mkboard (M.fromList[((m,n), Occupied side)] `M.union` board) size , True)







sampleinit :: [((Integer,Integer),  Blockstat)]
sampleinit = [((1,0), Occupied White),((2,0), Occupied White),((3,0), Occupied White),((4,0), Occupied White),((5,0), Occupied White), ((6,0), Occupied White), ((7,0), Occupied White), ((2,4), Occupied White), ((2,5), Occupied Black), ((3,6), Occupied Black), ((4,7), Occupied Black), ((0,6), Occupied Black)]
