import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8

import System.Environment (getArgs)

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
  msg <- B8.getLine
  send sock msg
  let ymove = B8.pack( "your move is " ++ B8.unpack msg ++ "\n")
  B8.putStrLn ymove

  msg0 <- recv sock 1000

  B8.putStrLn msg0

  msg1 <- recv sock 1000

  B8.putStrLn msg1

-- accept game signal
-- If (signal of ending, player win/lose) then (end game with UI) else
  msgSender1 sock


msgSender2 :: Socket -> IO ()
msgSender2 sock = do

  msg0 <- recv sock 1000

  B8.putStrLn msg0

  msg <- B8.getLine
  send sock msg

  let ymove = B8.pack( "your move is " ++ B8.unpack msg ++ "\n")
  B8.putStrLn ymove


  msg1 <- recv sock 1000

  B8.putStrLn msg1

-- accept game signal
-- If (signal of ending, player win/lose) then (end game with UI) else
  msgSender2 sock