module UI where

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Widget
  , customMain, neverShowCursor
  , halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )

import Board as GB
import Control as GC
import qualified Brick as B
import qualified Brick.Widgets.Border as WB
import qualified Brick.Widgets.Center as WC
import qualified Brick.Widgets.Edit as WE
import qualified Brick.Forms as BF
import qualified Board as GB
-- import Lens.Micro.TH (makeLenses)
-- import Lens.Micro ((&), (.~), (%~), (^.))
-- import qualified Graphics.Vty as V


data Game = Game
  { _board  :: GB.Board -- ^ Gomoku Board
  , _over   :: Bool     -- ^ game over
  , _winner :: GB.Side  -- ^ winner of the game
  , _current :: GB.Side
  } deriving (Show)

type Name = ()

-- black and white stoness for gomoku game

blockB, blockW :: B.Widget n
blockB = B.str "⚫"
blockW = B.str "⚪"

drawUI g = 
  [ WC.center $ padRight (Pad 2) (drawStats g) <+> drawBoard g ]

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
  $ vBox [ drawScore (g ^. over) (g ^. winner)]

drawScore :: Bool -> GB.Side -> Widget Name
drawScore over winner = 
  if over 
    then withBorderStyle BS.unicodeBold
      $ WB.borderWithLabel (str "Winner")
      $ WC.hCenter
      $ padAll 1
      $ str $ show winner
    else emptyWidget


drawBoard :: Game -> Widget Name
drawBoard g = withBorderStyle BS.unicodeBold
  $ WB.borderWithLabel (str "Gomoku")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c `elem` g ^. snake = Snake
      | c == g ^. food      = Food
      | otherwise           = Empty

drawCell :: GB.Blockstat -> Widget Name
drawCell (Occupied Black) = withAttr blackAttr cw
drawCell (Occupied White)  = withAttr whiteAttr cw
drawCell Empty = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

blackAttr, whiteAttr, emptyAttr :: AttrName
blackAttr = "blackAttr"
whiteAttr = "whiteAttr"
emptyAttr = "emptyAttr"









