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
import Control.Concurrent.MVar

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
   
    forkIO $ boardInteract conn1 conn2
    sockHandler n sock

boardInteract :: Socket -> Socket -> IO ()
boardInteract conn1 conn2 = do
    putStrLn $ "reciving message conn1 "
    msg <- recv conn1 1000
    putStrLn $ "reciving message conn1 over"
    let c1 = B8.unpack msg
    let cs = words c1


    if length cs /= 0 then do 
        let m = read (cs !! 0) :: Int
        let n = read (cs !! 1) :: Int  
        -- let newBoard = fst (addMove oldBoard m n White)
        putStrLn ("Black move" ++ c1)
        -- putStrLn (showBoard newBoard)
        send conn2 msg
    else do
        putStrLn $ "no message from conn1 "
        send conn2 msg



    putStrLn $ "reciving message conn2 "
    msg2 <- recv conn2 1000 
    let c2 = B8.unpack msg2
    let cs2 = words c2


    if length cs /= 0 then do 
        let m2 = read (cs2 !! 0) :: Int
        let n2 = read (cs2 !! 1) :: Int   
        -- let newBoard2 = fst (addMove newBoard m2 n2 Black)
        -- putStrLn ("white move" ++ c1)
        -- putStrLn (showBoard newBoard)
        send conn2 msg
        putStrLn ("White move" ++ c2)
        -- putStrLn (showBoard newBoard2)
        send conn1 msg2
    else do
        putStrLn $ "no message from conn2"
        send conn1 msg2

    boardInteract conn1 conn2 

