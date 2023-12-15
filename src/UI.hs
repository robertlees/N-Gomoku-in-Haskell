{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module UI where
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Board
import Control
import Network.Socket hiding (send, recv)
import Brick
import Brick.BChan
import Brick.Widgets.Table
import Brick.Widgets.Center (center)
import Data.Universe.Helpers
import Data.List.Split
import Data.List
import qualified Data.Map as M
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import qualified Data.Text as T
import Graphics.Vty.CrossPlatform (mkVty)
import qualified Graphics.Vty as V

import Control.Monad (forever, void)
import Control.Monad.IO.Class 
import Control.Concurrent 

data Stone = StoneB | StoneW | ValidCursor | InvalidCursor | StoneEmpty

mapStoneTxt :: Stone -> Widget ()
mapStoneTxt StoneB = txt "⬤"
mapStoneTxt StoneW = txt "〇"
mapStoneTxt StoneEmpty = txt ""
mapStoneTxt ValidCursor = txt "✔"
mapStoneTxt InvalidCursor = txt "✘"

mapPlayer :: Int -> Side
mapPlayer 1 = Black
mapPlayer 2 = White

mapStone :: Blockstat -> Stone
mapStone (Occupied White) = StoneW
mapStone (Occupied Black) = StoneB
mapStone _ = StoneEmpty

mapCursor :: Stone -> Stone
mapCursor StoneEmpty = ValidCursor
mapCursor stone = stone

elemToList :: a -> [a]
elemToList e = [e]

appendNum :: [[a]] -> [a] -> [[a]]
appendNum [] lb = []
appendNum la [] = la
appendNum la lb = case ((length la) == (length lb)) of
  True -> zipWith (++) (la) (fmap elemToList lb) 
  False -> []

drawUI :: GameState -> [Widget ()]
drawUI g = [center $ renderTable leftTable <+>
              padLeft (Pad 5) (renderTable rightTableA <=>
                               renderTable rightTableB)]
  where
    leftTable = drawBoard g
    rightTableA = drawPlayer g
    rightTableB = drawWinner g

drawBoard :: GameState -> Table ()
drawBoard g = 
  setDefaultColAlignment AlignCenter $
  table rsb
  where
    rsb =  glst ++ [flst]
    glst = appendNum olst flst2
    flst = [txt "  1 ", txt "  2 ", txt "  3 ", txt "  4 ", txt "  5 ", txt "  6 ", txt "  7 ", txt "  8 ",txt "  9 ", txt "  10", txt "  11", txt "  12", txt "  13", txt "  14", txt "  15",txt "    "]
    flst2 = [txt "1", txt "2", txt "3", txt "4", txt "5", txt "6", txt "7", txt "8",txt "9", txt "10", txt "11", txt "12", txt "13", txt "14", txt "15"]
    olst = chunksOf 15 (olst2)
    olst2 = fmap mapStoneTxt olst1
    olst1 = M.elems (M.adjust mapCursor csor olst0)
    olst0 = M.map mapStone bmap
    csor = g ^. cursor
    bmap =  getMap (g ^. board)
    getMap (Mkboard bd sz) = bd

drawPlayer :: GameState -> Table()
drawPlayer g = 
  columnBorders False $
  setDefaultColAlignment AlignCenter $
  table [ [txt "Current Player:",  cplayer]
          , [txt "Your Stone:", splayer]
          , [txt "Good Luck !", txt "    "]
          ]
    where
      cplayer = mapStoneTxt(mapStone (Occupied (g ^. player)))
      splayer = if (_skt g) == Nothing then cplayer else mapStoneTxt(mapStone (Occupied (g ^. slf)))

drawWinner :: GameState -> Table()
drawWinner g = 
  setDefaultColAlignment AlignCenter $
  table [ [txt gstr,    txt (T.pack (" " ++ (show (g ^. rule)) ++ " ")) ]
          , [txt wstr, mapStoneTxt(mapStone ((g ^. winner)))]
          ]
    where
      gstr = "Rule"
      wstr = if (g ^. winner) == Empty then "" else "Winner"


-- uiMain :: IO ()
-- uiMain = do
  
--   let exampleBoard = initBoard sampleinit 15
--       new =boardToStrings exampleBoard
--       g = Game exampleBoard 5 (Occupied White) White Black (2,3) True Nothing
  
--   simpleMain (drawUI g)
--   putStr $ showBoard exampleBoard
--   print $ boardToStrings exampleBoard
--   print (diagonals $ reverse new)
--   print (diagonals new)
--   print (checkWinB new 3)
--   print (checkWin exampleBoard 5)
--   let g2 = g { _rule=6 }
--   print (_rule g2)

aMap :: AttrMap
aMap = attrMap V.defAttr []

appUI :: App GameState Tick ()
appUI = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const aMap
          }

uiMain :: Maybe Socket -> Int -> Int ->  IO ()
uiMain sock n s = do
  
  let exampleBoard = initBoard sampleinit 15
      new =boardToStrings exampleBoard
      g = Game (emptyBoard 15) n (Empty) (mapPlayer s) Black (2,3) False sock
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 10000

  ivty <- mkVty V.defaultConfig
  void $ customMain ivty (mkVty V.defaultConfig) (Just chan) appUI g

  -- simpleMain (drawUI g)
  -- putStr $ showBoard exampleBoard
  -- print $ boardToStrings exampleBoard
  -- print (diagonals $ reverse new)
  -- print (diagonals new)
  -- print (checkWinB new 3)
  -- print (checkWin exampleBoard 7)
  -- print (_slf g)
  -- print (_player g)
  -- let g2 = g { _rule=6 }
  -- print (_rule g2)
