module Board where
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



pint :: Num b1 => ((a, b1), b2) -> ((a, b1), b2)
pint ((a, b), c ) = ((a, b+1 ), c) 








sampleinit :: [((Integer,Integer),  Blockstat)]
sampleinit = [((1,4), Occupied White), ((1,5), Occupied White), ((1,6), Occupied White), ((1,7), Occupied White), ((2,4), Occupied White), ((2,5), Occupied Black), ((3,6), Occupied Black), ((4,7), Occupied Black), ((0,6), Occupied Black)]





-- >>> putStr (showBoard (initBoard sampleinit 9))
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

bd1 :: Board
bd1 = fst (addMove(initBoard sampleinit 15) 1 3 Black)

getMap :: Board -> M.Map (Int, Int) Blockstat
getMap (Mkboard bd sz) = bd

bd2 :: M.Map (Int, Int) Blockstat
bd2 = getMap bd1

elemToList :: a -> [a]
elemToList e = [e]

tx :: Integer
tx = if 1 > 2 then 3 else 4

-- >>> M.toList bd2
-- [((0,0),Empty),((0,1),Empty),((0,2),Empty),((0,3),Empty),((0,4),Empty),((0,5),Empty),((0,6),Occupied Black),((0,7),Empty),((0,8),Empty),((0,9),Empty),((0,10),Empty),((0,11),Empty),((0,12),Empty),((0,13),Empty),((0,14),Empty),((1,0),Empty),((1,1),Empty),((1,2),Empty),((1,3),Occupied Black),((1,4),Occupied White),((1,5),Occupied White),((1,6),Occupied White),((1,7),Occupied White),((1,8),Empty),((1,9),Empty),((1,10),Empty),((1,11),Empty),((1,12),Empty),((1,13),Empty),((1,14),Empty),((2,0),Empty),((2,1),Empty),((2,2),Empty),((2,3),Empty),((2,4),Occupied White),((2,5),Occupied Black),((2,6),Empty),((2,7),Empty),((2,8),Empty),((2,9),Empty),((2,10),Empty),((2,11),Empty),((2,12),Empty),((2,13),Empty),((2,14),Empty),((3,0),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty),((3,4),Empty),((3,5),Empty),((3,6),Occupied Black),((3,7),Empty),((3,8),Empty),((3,9),Empty),((3,10),Empty),((3,11),Empty),((3,12),Empty),((3,13),Empty),((3,14),Empty),((4,0),Empty),((4,1),Empty),((4,2),Empty),((4,3),Empty),((4,4),Empty),((4,5),Empty),((4,6),Empty),((4,7),Occupied Black),((4,8),Empty),((4,9),Empty),((4,10),Empty),((4,11),Empty),((4,12),Empty),((4,13),Empty),((4,14),Empty),((5,0),Empty),((5,1),Empty),((5,2),Empty),((5,3),Empty),((5,4),Empty),((5,5),Empty),((5,6),Empty),((5,7),Empty),((5,8),Empty),((5,9),Empty),((5,10),Empty),((5,11),Empty),((5,12),Empty),((5,13),Empty),((5,14),Empty),((6,0),Empty),((6,1),Empty),((6,2),Empty),((6,3),Empty),((6,4),Empty),((6,5),Empty),((6,6),Empty),((6,7),Empty),((6,8),Empty),((6,9),Empty),((6,10),Empty),((6,11),Empty),((6,12),Empty),((6,13),Empty),((6,14),Empty),((7,0),Empty),((7,1),Empty),((7,2),Empty),((7,3),Empty),((7,4),Empty),((7,5),Empty),((7,6),Empty),((7,7),Empty),((7,8),Empty),((7,9),Empty),((7,10),Empty),((7,11),Empty),((7,12),Empty),((7,13),Empty),((7,14),Empty),((8,0),Empty),((8,1),Empty),((8,2),Empty),((8,3),Empty),((8,4),Empty),((8,5),Empty),((8,6),Empty),((8,7),Empty),((8,8),Empty),((8,9),Empty),((8,10),Empty),((8,11),Empty),((8,12),Empty),((8,13),Empty),((8,14),Empty),((9,0),Empty),((9,1),Empty),((9,2),Empty),((9,3),Empty),((9,4),Empty),((9,5),Empty),((9,6),Empty),((9,7),Empty),((9,8),Empty),((9,9),Empty),((9,10),Empty),((9,11),Empty),((9,12),Empty),((9,13),Empty),((9,14),Empty),((10,0),Empty),((10,1),Empty),((10,2),Empty),((10,3),Empty),((10,4),Empty),((10,5),Empty),((10,6),Empty),((10,7),Empty),((10,8),Empty),((10,9),Empty),((10,10),Empty),((10,11),Empty),((10,12),Empty),((10,13),Empty),((10,14),Empty),((11,0),Empty),((11,1),Empty),((11,2),Empty),((11,3),Empty),((11,4),Empty),((11,5),Empty),((11,6),Empty),((11,7),Empty),((11,8),Empty),((11,9),Empty),((11,10),Empty),((11,11),Empty),((11,12),Empty),((11,13),Empty),((11,14),Empty),((12,0),Empty),((12,1),Empty),((12,2),Empty),((12,3),Empty),((12,4),Empty),((12,5),Empty),((12,6),Empty),((12,7),Empty),((12,8),Empty),((12,9),Empty),((12,10),Empty),((12,11),Empty),((12,12),Empty),((12,13),Empty),((12,14),Empty),((13,0),Empty),((13,1),Empty),((13,2),Empty),((13,3),Empty),((13,4),Empty),((13,5),Empty),((13,6),Empty),((13,7),Empty),((13,8),Empty),((13,9),Empty),((13,10),Empty),((13,11),Empty),((13,12),Empty),((13,13),Empty),((13,14),Empty),((14,0),Empty),((14,1),Empty),((14,2),Empty),((14,3),Empty),((14,4),Empty),((14,5),Empty),((14,6),Empty),((14,7),Empty),((14,8),Empty),((14,9),Empty),((14,10),Empty),((14,11),Empty),((14,12),Empty),((14,13),Empty),((14,14),Empty)]

-- >>> M.elems bd2
-- [Empty,Empty,Empty,Empty,Empty,Empty,Occupied Black,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Occupied Black,Occupied White,Occupied White,Occupied White,Occupied White,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Occupied White,Occupied Black,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Occupied Black,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Occupied Black,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]

-- >>> zipWith (++) (fmap elemToList [1,2]) (fmap elemToList [2,4])
-- [[1,2],[2,4]]
