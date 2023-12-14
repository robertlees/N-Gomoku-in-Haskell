import Network.Socket hiding (send, recv)
import Network.Socket.ByteString 
import qualified Data.ByteString.Char8 as B8
import System.Environment (getArgs)

import System.IO 
import Control.Concurrent (forkIO)


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
addMove (Mkboard board size) m n side =  if isOccupied board m n then (Mkboard board size, False)
                                        else  if outOfBoard m n size  then (Mkboard board size, False)
                                        else   (Mkboard (M.fromList[((m,n), Occupied side)] `M.union` board) size , True)



pint :: Num b1 => ((a, b1), b2) -> ((a, b1), b2)
pint ((a, b), c ) = ((a, b+1 ), c) 










main :: IO ()
main = withSocketsDo $ do
    sock <- socket AF_INET Stream 0 
    bind sock (SockAddrInet 4000 0x0100007f)
    listen sock 2   
    putStrLn $ "Listening on 4000"  
    sockHandler sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
    let n = 5
    (conn1, _) <- accept sock  
    (conn2, _) <- accept sock  


    send conn1 (B8.pack (show n ++ " 1"))

    send conn2 (B8.pack (show n ++ " 2"))
   
    forkIO $ boardInteract conn1 conn2 (initBoard [] 15)
    sockHandler sock

boardInteract :: Socket -> Socket -> Board -> IO ()
boardInteract conn1 conn2 oldBoard = do

    msg <- recv conn1 1000 
    let c1 = B8.unpack msg
    let cs = words c1



    let m = read (cs !! 0)
    let n = read (cs !! 1)    



    let newBoard = fst (addMove oldBoard m n White)



    putStrLn ("white move" ++ c1)
    putStrLn (showBoard newBoard)

    send conn2 msg




    msg2 <- recv conn2 1000 
    let c2 = B8.unpack msg2
    let cs2 = words c2



    let m2 = read (cs2 !! 0)
    let n2 = read (cs2 !! 1)    



    let newBoard2 = fst (addMove newBoard m2 n2 Black)



    putStrLn ("black move" ++ c2)
    putStrLn (showBoard newBoard2)

    send conn1 msg2

    boardInteract conn1 conn2 newBoard2


