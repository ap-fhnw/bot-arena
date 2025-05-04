{-# LANGUAGE OverloadedStrings #-}
-- | Minimal Bot‑Arena grid renderer
--   Updated for modern **Brick ≥ 1.0** (no `Next`, no `continue`).
--   Build & run:
--     cabal build && cabal run bot-arena-tui          
--   Quit with the *q* key.
module Main where

import Control.Monad (void)
import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center)
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import System.Environment (getArgs)

-- Model

data Cell = Empty | Bot1 | Bot2 deriving (Eq, Show)

type Row  = Vec.Vector Cell

type Grid = Vec.Vector Row

newtype St = St { grid :: Grid }

-- | Resource names, one per cell (row, col).
newtype Name = Name (Int, Int) deriving (Eq, Ord, Show)

-- Pure helpers (arena logic)

makeBlankGrid :: Int -> Int -> Grid
makeBlankGrid rows cols = Vec.replicate rows (Vec.replicate cols Empty)

placeBot :: (Int, Int) -> Cell -> Grid -> Grid
placeBot (r, c) bot g
  | r < 0 || r >= Vec.length g            = g
  | c < 0 || c >= Vec.length (g Vec.! r)  = g
  | otherwise                             = g Vec.// [(r, row')]
  where
    row   = g Vec.! r
    row'  = row Vec.// [(c, bot)]

-- Rendering helpers

renderCell :: Cell -> Widget Name
renderCell Empty = padAll 0 $ str " · "
renderCell Bot1  = withAttr bot1Attr $ padAll 0 $ str " ⚔ "
renderCell Bot2  = withAttr bot2Attr $ padAll 0 $ str " ☠ "


renderGrid :: Grid -> Widget Name
renderGrid g = vBox
  [ hBox [ clickable (Name (r, c)) (renderCell (g Vec.! r Vec.! c))
         | c <- [0 .. cols - 1] ]
  | r <- [0 .. rows - 1] ]
  where
    rows = Vec.length g
    cols = Vec.length (Vec.head g)

-- Brick application definition

drawUI :: St -> [Widget Name]
drawUI st = [ center . border $ renderGrid (grid st) ]

-- | Handle keyboard events.
handleEvent :: BrickEvent Name e -> EventM Name St ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent _                                     = return ()

app :: App St e Name
app = App
  { appDraw         = drawUI
  , appStartEvent   = return ()
  , appHandleEvent  = handleEvent
  , appChooseCursor = neverShowCursor
  , appAttrMap      = const theMap
  }

-- Attribute map (colours & styles)

bot1Attr, bot2Attr :: AttrName
bot1Attr = attrName "bot1"
bot2Attr = attrName "bot2"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (bot1Attr, V.defAttr `V.withBackColor` V.blue `V.withForeColor` V.white)
  , (bot2Attr, V.defAttr `V.withBackColor` V.red  `V.withForeColor` V.white)
  ]

-- Main

main :: IO ()
main = do
  args <- getArgs
  let (rows, cols) = case args of
        [r, c] -> (read r, read c)
        _      -> (10, 10)
  let g0 = makeBlankGrid rows cols
      g1 = placeBot (2, 3) Bot1 g0
      g2 = placeBot (5, 7) Bot2 g1
  void $ defaultMain app (St g2)
