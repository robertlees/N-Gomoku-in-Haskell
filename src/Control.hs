module Control where
import Brick
import Control.Monad


-- needs a data class called player 

data GameState = GameState { currentPlayer :: Player, board :: [[Maybe Player]] }


initialState :: GameState
initialState = GameState { currentPlayer = PlayerX, board = replicate 5 (replicate 5 Nothing) }

handleEvent :: GameState -> BrickEvent () e -> EventM () (Next GameState)
handleEvent gs (VtyEvent (EvKey key _)) =
    case key of
        KChar 'q' -> halt gs
        KEsc -> halt gs
        KChar ' ' -> continue $ makeMove gs
        KLeft -> continue $ moveCursor gs (-1)
        KRight -> continue $ moveCursor gs 1
        KChar 'x' | currentPlayer gs == PlayerX -> continue $ makeMove gs
        KChar 'o' | currentPlayer gs == PlayerO -> continue $ makeMove gs
        _ -> continue gs
handleEvent gs _ = continue gs


makeMove :: GameState -> GameState
makeMove gs =
    let col = findEmptyColumn (board gs)
        updatedBoard = updateBoard (currentPlayer gs) col (board gs)
    in GameState { currentPlayer = togglePlayer (currentPlayer gs), board = updatedBoard }

findEmptyColumn :: [[Maybe Player]] -> Int
findEmptyColumn board = fromMaybe 0 $ findIndex (any isNothing) (transpose board)

moveCursor :: GameState -> Int -> GameState
moveCursor gs dir =
    let col = selectedColumn gs + dir
        maxCol = length (head (board gs)) - 1
        newCol = max 0 (min maxCol col)
    in gs { selectedColumn = newCol }
  
updateBoard :: Player -> Int -> [[Maybe Player]] -> [[Maybe Player]]
updateBoard player col board =
    let (before, (row:after)) = splitAt col board
        updatedRow = updateRow player row
    in before ++ [updatedRow] ++ after
