module UI where
    
import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Edit as B
import qualified Brick.Forms as B


-- black and white stoness for gomoku game

blockB, blockW :: Widget n
blockB = str "⚫"
blockW = str "⚪"





