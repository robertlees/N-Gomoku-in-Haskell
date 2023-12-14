module Main where

import System.Environment
import qualified Data.ByteString.Char8 as B8
import Data.List ()
import Board
import Client
import Control
import Server
import UI 

main :: IO ()
main = do
    args <- getArgs
    angency args

agency :: [a] -> IO()
agency ["server"] = do
    putStrLn $ "Welcome to the N-Gomoku server"
    rn <- getRule
    severMain rn

agency ["client"] = do
    putStrLn $ "Welcome to the N-Gomoku client"
    (socket,n) <- clientMain
    uiMain socket n


getRule = do
  putStrLn $ "Please define the rule of the N-Gomoku game"
  putStrLn $ "format: integer within [5,12]"
  rn <- getLine
  let rn2 = reads rn :: [(Integer, String)]

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