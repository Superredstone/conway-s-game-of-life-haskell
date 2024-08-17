{-# LANGUAGE TupleSections #-}

module Lib
  ( initializeCells,
    vector2Scale,
    vector2Add,
    vector2Divide,
    handleCameraMovement,
    drawCells,
    nextCellGeneration,
    liveNeighborCount,
    handleUpdateTickSpeed,
    handleUpdateMaxFps,
    cellSize,
    mousePositionToGrid,
    handleCameraZoom,
  )
where

import Control.Monad (forM_)
import Data.HashMap
import GHC.Base (when)
import Raylib.Core (getMouseDelta, isKeyDown, isKeyPressed, isMouseButtonDown, setTargetFPS)
import Raylib.Core.Shapes (drawRectangle)
import Raylib.Types
import Raylib.Util.Colors (red)

cellSize :: Int
cellSize = 25

initializeCells :: Map (Int, Int) Bool
initializeCells = fromList []

vector2Scale :: Vector2 -> Float -> Vector2
vector2Scale v factor = Vector2 {vector2'x = vector2'x v * factor, vector2'y = vector2'y v * factor}

vector2Add :: Vector2 -> Vector2 -> Vector2
vector2Add v1 v2 = Vector2 {vector2'x = vector2'x v1 + vector2'x v2, vector2'y = vector2'y v1 + vector2'y v2}

vector2Divide :: Vector2 -> Vector2 -> Vector2
vector2Divide v1 v2 = Vector2 {vector2'x = vector2'x v1 / vector2'x v2, vector2'y = vector2'y v1 / vector2'y v2}

handleCameraMovement :: Camera2D -> IO Camera2D
handleCameraMovement camera = do
  rightMouseButton <- isMouseButtonDown MouseButtonRight
  mouseDelta <- getMouseDelta
  let delta = vector2Scale mouseDelta (-(1.0 / camera2D'zoom camera))

  if rightMouseButton
    then return camera {camera2D'target = vector2Add (camera2D'target camera) delta}
    else return camera

drawCells :: Map (Int, Int) Bool -> IO ()
drawCells a = forM_ (toList a) (uncurry drawCell)

drawCell :: (Int, Int) -> Bool -> IO ()
drawCell (x, y) v =
  when
    v
    ( drawRectangle (x * cellSize) (y * cellSize) cellSize cellSize red
    )

getNeighbors :: (Int, Int) -> [(Int, Int)]
getNeighbors (x, y) =
  [ (x - 1, y - 1), -- Top left
    (x - 1, y), -- Middle left
    (x - 1, y + 1), -- Bottom left
    (x, y - 1), -- Bottom center
    (x, y + 1), -- Top center
    (x + 1, y - 1), -- Bottom right
    (x + 1, y), -- Center right
    (x + 1, y + 1) -- Top right
  ]

liveNeighborCount :: Map (Int, Int) Bool -> (Int, Int) -> Int
liveNeighborCount grid pos = foldr (\n acc -> if findWithDefault False n grid then acc + 1 else acc) 0 (getNeighbors pos)

nextCellState :: Map (Int, Int) Bool -> (Int, Int) -> Bool
nextCellState grid pos =
  let currentState = findWithDefault False pos grid
      liveNeighbors = liveNeighborCount grid pos
   in case (currentState, liveNeighbors) of
        (True, n) | n < 2 -> False -- Underpopulation
        (True, 2) -> True -- Lives on to the next generation
        (True, 3) -> True -- Lives on to the next generation
        (True, n) | n > 3 -> False -- Overpopulation
        (False, 3) -> True -- Reproduction
        _ -> currentState -- Remains in the current state

nextCellGeneration :: Map (Int, Int) Bool -> Map (Int, Int) Bool
nextCellGeneration grid =
  let allCells = keys grid ++ concatMap getNeighbors (keys grid)
      uniqueCells = fromList $ Prelude.map (,False) allCells
      nextGen = mapWithKey (\pos _ -> nextCellState grid pos) (grid `union` uniqueCells)
   in Data.HashMap.filter id nextGen

handleUpdateTickSpeed :: Integer -> IO Integer
handleUpdateTickSpeed t = do
  keyD <- isKeyDown KeyD
  keyA <- isKeyDown KeyA

  if keyD
    then return (t + 1)
    else
      if keyA && t > 1
        then return (t - 1)
        else return t

handleUpdateMaxFps :: Integer -> IO Integer
handleUpdateMaxFps fps = do
  keyN <- isKeyPressed KeyN

  let newMaxFps =
        ( case (keyN, fps) of
            (False, _) -> fps
            (True, 30) -> 60
            (True, 60) -> 144
            (True, 144) -> 30
            (True, _) -> 30
        )

  when keyN (setTargetFPS (fromIntegral newMaxFps))

  return newMaxFps

handleCameraZoom :: Camera2D -> IO Camera2D
handleCameraZoom c = do
  keyZ <- isKeyPressed KeyZ

  let newZoom =
        ( case (keyZ, camera2D'zoom c) of
            (False, _) -> camera2D'zoom c
            (True, 0.5) -> 1.0
            (True, 1.0) -> 1.5
            (True, 1.5) -> 0.5
            (True, _) -> 1.0
        )

  return c {camera2D'zoom = newZoom}

mousePositionToGrid :: Vector2 -> Camera2D -> Vector2
mousePositionToGrid mousePos camera = do
  let mouseWorldPos = vector2Add (vector2Divide mousePos (Vector2 (camera2D'zoom camera) (camera2D'zoom camera))) (camera2D'target camera)
  vector2Divide mouseWorldPos (fromIntegral cellSize)
