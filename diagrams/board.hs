
import qualified Data.Map as M
import Data.List ()
import Data.String ()

import Data.Char

data Side = Black
            | White
            deriving (Eq, Show)            

data Blockstat = Occupied Side
                | Empty 
                deriving (Eq, Show)

data Board = Mkboard (M.Map (Int, Int) Blockstat) Int  deriving(Show)

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


addMove :: Board -> Int -> Int -> Side -> (Board, Bool)
addMove (Mkboard board size) m n side =  if isOccupied board m n then (Mkboard board size, False)
                                   else     (Mkboard (M.fromList[((m,n), Occupied side)] `M.union` board) size , True)








sampleinit :: [((Integer,Integer),  Blockstat)]
sampleinit = [((1,4), Occupied White), ((1,5), Occupied White), ((1,6), Occupied White), ((1,7), Occupied White), ((2,4), Occupied White), ((2,5), Occupied Black), ((3,6), Occupied Black), ((4,7), Occupied Black), ((0,6), Occupied Black)]





-- >>> putStr (showBoard (initBoard sampleinit 8))
--   0 1 2 3 4 5 6 7
-- 0 - - - - - - x - 0
-- 1 - - - - o o o o 1
-- 2 - - - - o x - - 2
-- 3 - - - - - - x - 3
-- 4 - - - - - - - x 4
-- 5 - - - - - - - - 5
-- 6 - - - - - - - - 6
-- 7 - - - - - - - - 7
--   0 1 2 3 4 5 6 7
--




-- >>> putStr (showBoard (fst (addMove(initBoard sampleinit 8) 1 3 Black)) )
--   0 1 2 3 4 5 6 7
-- 0 - - - - - - x - 0
-- 1 - - - x o o o o 1
-- 2 - - - - o x - - 2
-- 3 - - - - - - x - 3
-- 4 - - - - - - - x 4
-- 5 - - - - - - - - 5
-- 6 - - - - - - - - 6
-- 7 - - - - - - - - 7
--   0 1 2 3 4 5 6 7
--
