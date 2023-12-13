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
import GHC.Windows (ddwordToDwords)

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










main :: IO ()
main = withSocketsDo $ do
    sock <- socket AF_INET Stream 0 
    bind sock (SockAddrInet 4000 0x0100007f)
    listen sock 2   
    putStrLn $ "Listening on 4000"  
    sockHandler sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
    (conn1, _) <- accept sock  
    (conn2, _) <- accept sock  
    send conn1 (B8.pack "1")
    send conn2 (B8.pack "2")
    forkIO $ boardInteract conn1 conn2 (initBoard [] 8) -- Board size init here
    sockHandler sock





boardInteract :: Socket -> Socket -> Board -> IO ()
boardInteract conn1 conn2 oldBoard = do

    (newBoard, ended )<- oneMove conn1 conn2 oldBoard White

    if ended == "0" then do
                            (newBoard2, ended2 )<- oneMove conn2 conn1 newBoard Black

                            if ended2 =="0" then boardInteract conn1 conn2 newBoard2
                                            else putStrLn "Game ends \n"
                    else
                            putStrLn "Game ends \n"










oneMove :: Socket -> Socket -> Board -> Side -> IO (Board, String)
oneMove conn1 conn2 board side = do

    msg <- recv conn1 1000 
                   

    let c1 = B8.unpack msg
    let cs = words c1

    

    let m = read (cs !! 0)
    let n = read (cs !! 1)    


    let (newBoard, valid) = addMove board m n side

    putStrLn (show side ++ " move" ++ c1)
    putStrLn (showBoard newBoard)

    if valid  then do
                            let ended = "0" -- judge here whether game ends, 0 for game continue, 1 for white wins and 2 for black wins 

                            send conn1  (B8.pack ended)
                            send conn1  (B8.pack (showBoard newBoard ++ "\n")) 
                            -- currently sending back a string, which is board representation with ascii, UI can be added here, but make sure it's in one string or changes at client side is needed
                            send conn2  (B8.pack ended)
                            send conn2 (B8.pack( "opponent move " ++ c1 ++ "\n" ++ "\n" ++  (showBoard newBoard) ++ "\n" ) )
                            -- currently sending back a string, which is opponent move and board representation with ascii, UI can be added here,  but make sure it's in one string or changes at client side is needed
                            return (newBoard, ended)
                     else do
                            send conn1  (B8.pack ("-1"))
                            oneMove conn1 conn2 board side


