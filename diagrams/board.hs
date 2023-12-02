
import qualified Data.Map as M
import Data.List
import Data.String
import Data.Char

data Side = Black
            | White
            deriving (Eq, Show)            

data Blockstat = Occupied Side
                | Empty 
                deriving (Eq, Show)

data Board = Mkboard M.Map (Int, Int) Blockstat, Int

showBlock :: Blockstat -> String
showBlock b = case b of 
             White -> " o"
             Black -> " x"


pickRow :: Board -> Int -> Int -> Blockstat
pickRow (Mkboard board size) m n = board ! (m,n)


showRow :: Board -> Int -> String
showRow Bd@(Mkboard board size) m = show n ++ map showBlock (map (pickRow Bd m) [0..(size-1)]) ++ " " ++ show n

labelRow :: Int -> String
labelRow size = 
            if size==1 then 
            "0"
            else if (size < 10 && size > 1) then 
                 labelRow (size-1) ++ " " ++ show (size-1)
            else 
                 labelRow (size-1) ++ " " ++ chr (ord 'a' + (size-10))
showBoard :: Board -> String
showBoard Bd@(Mkboard board size) =
    labelRow size ++ (map (showRow Bd) [0..(size-1)]) ++ labelRow size
