{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Data.HashMap (insert)
import Lib (drawCells, handleCameraMovement, handleUpdateMaxFps, handleUpdateTickSpeed, initializeCells, mousePositionToGrid, nextCellGeneration)
import Raylib.Core
import Raylib.Core.Shapes (drawCircle)
import Raylib.Core.Text (drawText)
import Raylib.Types (Camera2D (Camera2D), MouseButton (MouseButtonLeft), vector2'x, vector2'y, pattern Vector2)
import Raylib.Util (whileWindowOpen_)
import Raylib.Util.Colors (black, rayWhite, red)

main :: IO ()
main = do
  window <- initWindow 800 800 "Conway's game of life"

  let defaultCamera = Camera2D (Vector2 0 0) (Vector2 0 0) 0 1
  let defaultCells = insert (2, 2) True $ insert (0, 1) True $ insert (1, 2) True $ insert (2, 0) True $ insert (2, 1) True initializeCells
  let defaultTicks = (1 :: Integer)
  let defaultMaxTicks = 5
  let defaultMaxFps = 60

  setTargetFPS defaultMaxFps

  whileWindowOpen_
    ( \(camera, cells, ticks, maxTicks, maxFps) -> do
        newCamera <- handleCameraMovement camera
        beginDrawing

        clearBackground rayWhite

        beginMode2D newCamera
        drawCells cells

        drawCircle 0 0 10 red

        endMode2D

        mousePos <- getMousePosition
        let mouse2grid = mousePositionToGrid mousePos newCamera

        drawText ("[A]/[D] Frame delay: " ++ show maxTicks) 10 10 30 black
        drawText ("[N] Max FPS: " ++ show maxFps) 10 50 30 black
        drawText ("Mouse world position: " ++ show mouse2grid) 10 90 30 black
        endDrawing

        newMaxTicks <- handleUpdateTickSpeed maxTicks
        newMaxFps <- handleUpdateMaxFps maxFps

        let (newCells, newTicks) =
              if mod ticks newMaxTicks == 0
                then (nextCellGeneration cells, 1)
                else (cells, ticks + 1)

        leftMouse <- isMouseButtonPressed MouseButtonLeft
        let newCellsUserInput = if leftMouse then insert (floor $ vector2'x mouse2grid, floor $ vector2'y mouse2grid) True newCells else newCells

        return (newCamera, newCellsUserInput, newTicks, newMaxTicks, newMaxFps)
    )
    (defaultCamera, defaultCells, defaultTicks, defaultMaxTicks, 60)

  closeWindow (Just window)
