module UI where


import Board as GB
import Control as GC
import Brick
import Brick.Widgets.Table
import Brick.Widgets.Center (center)
import Data.List.Split
import Data.List
import qualified Data.Map as M
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)

-- import qualified Graphics.Vty as V


data GameState = Game
  { _board  :: GB.Board -- ^ Gomoku Board
  , _rule   :: Int     -- ^ how many stones in a line will win the game?
  , _winner :: GB.Blockstat  -- ^ winner of the game
  , _player :: GB.Side -- ^ current player
  , _cursor  :: (Int, Int) -- ^ cursor's location
  , _socket :: Socket -- ^ network socket
  } deriving (Show)

ui :: Widget ()
ui = center $ renderTable leftTable <+>
              padLeft (Pad 5) (renderTable rightTableA <=>
                               renderTable rightTableC)

-- black and white stoness for gomoku game

stoneB, stoneW, validCursor, invalidCursor :: string
stoneB = "⏺"
stoneW = "🞅"
validCursor = "⊙"
invalidCursor = "⊗"

mapStone :: GB.Blockstat -> Widget ()
mapStone (Occupied White) = txt stoneW
mapStone (Occupied Black) = txt stoneB
mapStone _ = txt ""

mapCursor :: GB.Blockstat -> Widget ()
mapCursor Empty = txt validCursor
mapCursor _ = txt invalidCursor

elemToList :: a -> [a]
elemToList e = [e]

appendNum :: [a] -> [b] -> [b]
appendNum [] lb = lb
appendNum la [] = la
appendNum la lb = case ((length la) == (length lb)) of
  True -> zipWith (++) (fmap elemToList la) (fmap elemToList lb) 


drawUI :: Game -> Widget ()
drawUI g = center $ renderTable leftTable <+>
              padLeft (Pad 5) (renderTable rightTableA <=>
                               renderTable rightTableB)
  where
    leftTable = drawBoard g
    rightTableA = drawPlayer g
    rightTableB = drawWinner g

drawBoard :: Game -> Table ()
drawBoard g = 
  setDefaultColAlignment AlignCenter $
  table bd
  where
    bd = fmap mapStone glst
    glst = (appendNum olst [1..15]) ++ flst 
    flst = [txt "  1 ", txt "  2 ", txt "  3 ", txt "  4 ", txt "  5 ", txt "  6 ", txt "  7 ", txt "  8 ",txt "  9 ", txt "  10", txt "  11", txt "  12", txt "  13", txt "  14", txt "  15",txt "    "]
    olst = splitEvery 15 (M.elem (M.toList bmap))
    bmap =  getMap (g ^. board)
    getMap (Mkboard bd sz) = bd

drawPlayer :: Game -> Table()
drawPlayer g = 
  rowBorders False $
  setDefaultColAlignment AlignCenter $
  table [ [txt "Current",     mapStone (Occupied (g ^. player))]
          , [txt "player", txt "    "]
          ]

drawWinner :: Game -> Table()
drawWinner g = 
  surroundingBorder False $
  rowBorders False $
  columnBorders False $
  setDefaultColAlignment AlignCenter $
  table [ [txt gstr,     mapStone (g ^. winner)]
          , [txt wstr, txt "    "]
          ]
    where
      gstr = if (g ^. winner) == Empty then "" else "Game End"
      wstr = if (g ^. winner) == Empty then "" else "--> Winner"


uiMain :: Socket -> Int -> IO ()
uiMain socket n = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 10000
  g <- GC.initGame socket n
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app g








