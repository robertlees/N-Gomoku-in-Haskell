{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Client where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8
import Control.Concurrent.MVar



-----------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------



client :: IO (Socket, Int, Int)
client = withSocketsDo $ do
                addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just $ show 4000)
                let serverAddr = head addrInfo
                sock <- socket (addrFamily serverAddr) Stream defaultProtocol
                connect sock (addrAddress serverAddr)
                putStrLn "Waiting for the other player \n"
                msg1 <- recv sock 1000

                let n = words ( B8.unpack msg1 ) 
                
                let n2 = read ((n !! 0)::String)
                putStrLn (show (n2::Int))
                let n3 = read ((n !! 1)::String)
                putStrLn (show (n3::Int))

                return (sock, n2::Int, n3::Int )-- sock, game n, player 1/2



sender :: Socket -> String -> IO ()
sender sock str = do  --give a string "x y"
  send sock (B8.pack str)
  return ()



receiver :: Socket -> IO (Maybe (Int, Int))
receiver sock = do
                    msg0 <- recv sock 1000

                    let n = words ( B8.unpack msg0 )

                    if length n == 0 then do return Nothing
                    else do
                        let n2 = read ((n !! 0)::String)
                        putStrLn (show (n2::Int))
                        let n3 = read ((n !! 1)::String)
                        putStrLn (show (n3::Int))
                        return (Just (n2::Int, n3::Int ))-- x,y           