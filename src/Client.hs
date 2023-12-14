module Client where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8

import System.Environment (getArgs)
import GHC (HsDataDefn(dd_kindSig))

main :: IO ()
main = withSocketsDo $ do
                addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just $ show 4000)
                let serverAddr = head addrInfo
                sock <- socket (addrFamily serverAddr) Stream defaultProtocol
                connect sock (addrAddress serverAddr)
                putStrLn "Waiting for the other player \n"
                msg0 <- recv sock 1000
                if B8.unpack msg0 == "1" then do 
                                            putStrLn "you are first to move (o) \n"
                                            msgSender1 sock
                                          else do
                                            putStrLn "you are second to move (x) \n"
                                            msgSender2 sock
                


msgSender1 :: Socket -> IO ()
msgSender1 sock = do
  msg <- ownMove sock
  if msg == "0" then do
                    msg1 <- otherMove sock
                    if msg1 =="0" then do 
                                        msgSender1 sock
                                  else -- add player win/lose with signal 1 or 2, for player1 signal 1 is winning 
                                        putStrLn "Game ends \n"
                 else -- add player win/lose with signal 1 or 2, for player1 signal 1 is winning 
                    putStrLn "Game ends \n"


msgSender2 :: Socket -> IO ()
msgSender2 sock = do

  msg <- otherMove sock
  if msg == "0" then do
                    msg1 <- ownMove sock
                    if msg1 =="0" then do 
                                        msgSender2 sock
                                  else -- add player win/lose with signal 1 or 2, for player2 signal 2 is winning 
                                        putStrLn "Game ends \n"
                 else-- add player win/lose with signal 1 or 2, for player2 signal 2 is winning 
                    putStrLn "Game ends \n"




ownMove :: Socket -> IO String
ownMove sock = do

  msg <- B8.getLine
  send sock msg
  let ymove = B8.pack( "your move is " ++ B8.unpack msg ++ "\n")
  B8.putStrLn ymove

  msg0 <- recv sock 1000
  if B8.unpack msg0 == "-1" then do
                                  putStrLn "invalid move, please retry"
                                  ownMove sock
                            else do 
                                  msg1 <- recv sock 1000 --board after self move received here, change if needed by UI
                                  B8.putStrLn msg1
                                  return  (B8.unpack msg0)



otherMove :: Socket -> IO String
otherMove sock = do
                    msg0 <- recv sock 1000

                    msg1 <- recv sock 1000 --board after opponent move received here, change if needed by UI
                    B8.putStrLn msg1
                    return  (B8.unpack msg0)