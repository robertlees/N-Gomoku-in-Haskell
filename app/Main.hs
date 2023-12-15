{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Main where 

import System.Environment
import qualified Data.ByteString.Char8 as B8
import Data.List ()
import Board
import Control
import UI 
import Server
import Client

main :: IO ()
main = do
    args <- getArgs
    agency args

agency :: [String] -> IO()

agency l = do
    if l!!0 == "offline" then do
        putStrLn $ "Welcome to the N-Gomoku offline"
        rn <- getRule
        uiMain Nothing rn 1
        else if l!!0 == "server" then do
            putStrLn $ "Welcome to the N-Gomoku server"
            rn <- getRule
            serverMain rn
            else if l!!0 == "client" then do
                putStrLn $ "Welcome to the N-Gomoku client"
                (socket,n,s) <- client
                uiMain (Just socket) n s
                else do
                    putStrLn $ "Invalid input"
                    return ()


getRule = do
  putStrLn $ "Please define the rule of the N-Gomoku game"
  putStrLn $ "format: integer within [5,12]"
  rn <- getLine
  let rn2 = reads rn :: [(Int, String)]

  if ((rn2 == []) || (length rn2 > 1)) || (snd (rn2 !! 0) /= "")
    then do
      putStrLn $ "Illegal argument, try again"
      getRule
    else
      if (fst (rn2 !! 0) < 5) || (fst (rn2 !! 0) > 12)
        then do
          putStrLn $ "rule should be within [5,12], try again"
          getRule
        else do
          putStrLn $ "The rule is " ++ show (fst (rn2 !! 0))
          return (fst (rn2 !! 0))

-- main :: IO ()
-- main = do
--     uiMain