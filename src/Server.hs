module Server where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString 
import qualified Data.ByteString.Char8 as B8
import System.Environment (getArgs)

import System.IO 
import Control.Concurrent (forkIO)


import qualified Data.Map as M
import Data.List ()
import Data.String ()
import Board

import Data.Char
-- import GHC.Windows (ddwordToDwords)

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

