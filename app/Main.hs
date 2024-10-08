{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Data.HashMap (findWithDefault, insert)
import Lib (drawCells, handleCameraMovement, handleCameraZoom, handleUpdateMaxFps, handleUpdateTickSpeed, initializeCells, mousePositionToGrid, nextCellGeneration)
import Raylib.Core
import Raylib.Core.Shapes (drawCircle)
import Raylib.Core.Text (drawText)
import Raylib.Types (Camera2D (Camera2D, camera2D'zoom), ConfigFlag (WindowResizable), MouseButton (MouseButtonLeft), vector2'x, vector2'y, pattern Vector2)
import Raylib.Types.Core (KeyboardKey (KeySpace))
import Raylib.Util (whileWindowOpen_)
import Raylib.Util.Colors (black, rayWhite)

main :: IO ()
main = do
  setConfigFlags [WindowResizable]
  window <- initWindow 800 800 "Conway's game of life"

  let defaultCamera = Camera2D (Vector2 0 0) (Vector2 0 0) 0 1
      defaultCells = insert (2, 2) True $ insert (0, 1) True $ insert (1, 2) True $ insert (2, 0) True $ insert (2, 1) True initializeCells
      defaultTicks = (1 :: Integer)
      defaultMaxTicks = 5
      defaultMaxFps = 60
      defaultCameraZoom = (1.0 :: Float)

  setTargetFPS defaultMaxFps

  whileWindowOpen_
    ( \(camera, cells, pausing, ticks, maxTicks, maxFps, cameraZoom) -> do
        movedCamera <- handleCameraMovement camera
        beginDrawing

        clearBackground rayWhite

        beginMode2D movedCamera
        drawCells cells

        drawCircle 0 0 5 black

        endMode2D

        drawText ("[A]/[D] Frame delay: " ++ show maxTicks) 10 10 30 black
        drawText ("[Z] Zoom: " ++ show (camera2D'zoom movedCamera)) 10 50 30 black
        drawText ("[N] Max FPS: " ++ show maxFps) 10 90 30 black
        drawText ("[Space] Pause: " ++ show pausing) 10 130 30 black
        endDrawing

        newMaxTicks <- handleUpdateTickSpeed maxTicks
        newMaxFps <- handleUpdateMaxFps maxFps
        newCamera <- handleCameraZoom movedCamera
        let zoomFactor = camera2D'zoom newCamera

        leftMouse <- isMouseButtonPressed MouseButtonLeft
        spacePressed <- isKeyPressed KeySpace
        mousePos <- getMousePosition

        let (newCells, newTicks) =
              if mod ticks newMaxTicks == 0 && not pausing
                then (nextCellGeneration cells, 1)
                else (cells, ticks + 1)
            mouse2grid = mousePositionToGrid mousePos newCamera
            mouseXGrid = floor $ vector2'x mouse2grid
            mouseYGrid = floor $ vector2'y mouse2grid
            cellAtMousePos = findWithDefault False (mouseXGrid, mouseYGrid) newCells
            newCellsUserInput = if leftMouse then insert (mouseXGrid, mouseYGrid) (not cellAtMousePos) newCells else newCells
            newPausing = if spacePressed then not pausing else pausing

        return (newCamera, newCellsUserInput, newPausing, newTicks, newMaxTicks, newMaxFps, cameraZoom)
    )
    (defaultCamera, defaultCells, True, defaultTicks, defaultMaxTicks, 60, defaultCameraZoom)

  closeWindow (Just window)
