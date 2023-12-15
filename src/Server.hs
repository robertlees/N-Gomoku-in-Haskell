{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Server where

import Board
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



serverMain :: Int -> IO ()
serverMain n = withSocketsDo $ do
    sock <- socket AF_INET Stream 0 
    bind sock (SockAddrInet 4000 0x0100007f)
    listen sock 2   
    putStrLn $ "Listening on 4000"  
    sockHandler n sock

sockHandler :: Int -> Socket -> IO ()
sockHandler n sock = do
    -- let n = 5
    (conn1, _) <- accept sock  
    (conn2, _) <- accept sock  


    send conn1 (B8.pack (show n ++ " 1"))

    send conn2 (B8.pack (show n ++ " 2"))
   
    forkIO $ boardInteract conn1 conn2 (initBoard [] 15)
    sockHandler n sock

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

