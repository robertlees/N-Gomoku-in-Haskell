
import qualified Data.Map as M
import Data.List ()
import Data.String ()
import Test.QuickCheck 
import Data.Char

data Side = Black
            | White
            deriving (Eq, Show)            

data Blockstat = Occupied Side
                | Empty 
                deriving (Eq, Show)

data Board = Mkboard (M.Map (Int, Int) Blockstat) Int  deriving(Show, Eq)

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





prop_out :: Int -> Int -> Bool
prop_out a b = (a < 15) || snd (addMove (initBoard [] 15) a b Black) == False

prop_out2 :: Int -> Int -> Bool
prop_out2 a b = (a >=0) || snd (addMove (initBoard [] 15) a b Black) == False

prop_out3 :: Int -> Int -> Bool
prop_out3 a b = (b < 15) || snd (addMove (initBoard [] 15) a b Black) == False

prop_out4 :: Int -> Int -> Bool
prop_out4 a b = (b >= 0) || snd (addMove (initBoard [] 15) a b Black) == False

prop_init :: Int -> Int -> Bool
prop_init a b = outOfBoard a b 15 || (isOccupiedBoard (initBoard [] 15) a b == False)

prop_init2 :: Integer -> Integer -> Bool
prop_init2 a b = outOfBoard a b 15 || (isOccupiedBoard (initBoard [((a,b), Occupied Black)] 15) (fromIntegral a) (fromIntegral b) == True)

prop_add ::Int -> Int -> Bool
prop_add a b = outOfBoard a b 15 || (isOccupiedBoard (fst (addMove (initBoard [] 15) a b Black)) a b  == True)


prop_add2 ::Int -> Int -> Integer -> Integer -> Bool
prop_add2 a b c d = outOfBoard a b 15 || (isOccupiedBoard (fst (addMove (initBoard [((c,d), Occupied Black)] 15) a b Black)) a b  == True)

prop_empty :: Int -> Bool
prop_empty a = (a<=0) || emptyBoard a == initBoard [] a

-- >>> quickCheck prop_empty
-- +++ OK, passed 100 tests.
--

-- >>> quickCheck prop_out
-- +++ OK, passed 100 tests.
--


-- >>> quickCheck prop_out2
-- +++ OK, passed 100 tests.
--

-- >>> quickCheck prop_out3
-- +++ OK, passed 100 tests.
--

-- >>> quickCheck prop_out4
-- +++ OK, passed 100 tests.
--

-- >>> quickCheck prop_init
-- +++ OK, passed 100 tests.
--

-- >>> quickCheck prop_init2
-- +++ OK, passed 100 tests.
--

-- >>> quickCheck prop_add
-- +++ OK, passed 100 tests.
--

-- >>> quickCheck prop_add2
-- +++ OK, passed 100 tests.
--

isOccupied ::  M.Map (Int, Int) Blockstat -> Int -> Int -> Bool
isOccupied  board  m n  =   board M.! (m,n) /= Empty

isOccupiedBoard :: Board -> Int -> Int -> Bool
isOccupiedBoard   (Mkboard board size)  m n  =   isOccupied board m n

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

-- >>> putStr (showBoard (initBoard [] 8))
--   0 1 2 3 4 5 6 7
-- 0 - - - - - - - - 0
-- 1 - - - - - - - - 1
-- 2 - - - - - - - - 2
-- 3 - - - - - - - - 3
-- 4 - - - - - - - - 4
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
