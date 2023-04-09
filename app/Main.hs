module Main (main) where

import Lib
import Graphics.Gloss ( white, simulate, Display(InWindow) )

code :: Code
code = Lib.do
  turn N
  moveN 2

  turn E
  moveN 2

  turn S
  moveN 2

  turn W
  moveN 2


world :: Layout -> World
world l = defaultWorld l code

main :: IO ()
main = do
  f <- readFile "./assets/room"
  let l = toLayout $ lines f
  simulate (InWindow "Nice Window" (800, 600) (10, 10)) white 5 (world l) drawWorld (\_ _ w -> executeStep w)
