module Rendering (
                  rendering
                  ) where

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

import System.FilePath ((</>))

import Types
import Helper

-- Main rendering function, makes appropriate calls to rendering functions
rendering :: GameState -> AppEnv ()
rendering Intro               = introRendering
rendering Credits             = creditsRendering
rendering Menu                = menuRendering
rendering NewGame01           = newGame01Rendering
rendering NewGame02           = newGame02Rendering
rendering MenuSettings        = menuSettingsRendering
rendering _                   = error "rendering not handled!"



{-
***********************************************************************
*            Intro Rendering
***********************************************************************
-}
introRendering :: AppEnv ()
introRendering = do
  -- Gets resources
  screen <- getScreen
  bg     <- getIntroBg

  -- Blits
  liftIO $ applySurface 0 0 bg screen Nothing
  return ()



{-
***********************************************************************
*            Credits Rendering
***********************************************************************
-}
creditsRendering :: AppEnv ()
creditsRendering = do
  -- Gets resources
  screen <- getScreen
  bg     <- getCreditsBg

  -- Blits
  liftIO $ applySurface 0 0 bg screen Nothing
  return ()



{-
***********************************************************************
*            Menu Rendering
***********************************************************************
-}
menuRendering :: AppEnv ()
menuRendering = do
  -- Gets resources
  screen <- getScreen
  bg     <- getMenuBg
  arrow  <- getMenuArrow

  -- Gets arrow position
  pos    <- liftM fst getMenuSelector
  
  -- Computes the position of the arrow (hard-guessed from GIMP)
  let (x,y) = case pos of
                0 -> (58, 126)
                1 -> (58, 179)
                2 -> (58, 233)
                3 -> (58, 281)

  -- Blits
  liftIO $ do
    applySurface 0 0 bg screen Nothing
    applySurface x y arrow screen Nothing

  return ()



{-
***********************************************************************
*            NewGame01 Rendering
***********************************************************************
-}
newGame01Rendering :: AppEnv ()
newGame01Rendering = do
  -- Gets resources
  screen     <- getScreen
  newGameBg  <- liftIO $ loadImage (img </> "new_game_01.png")

  -- Blits
  liftIO $ do
    applySurface 0 0 newGameBg screen Nothing

  return ()



{-
***********************************************************************
*            NewGame02 Rendering
***********************************************************************
-}
newGame02Rendering :: AppEnv ()
newGame02Rendering = do
  problem2
  -- Bad idea: loadImage will be called at every loop!
  -- Better: use a Maybe type to store new game bgs in initEnv
  -- Initially init them to store the background
  -- Call freeSurface on them and set them to Nothing when
  -- changing from the menu to the game
  -- Gets resources
  screen     <- getScreen
  newGameBg  <- liftIO $ loadImage (img </> "new_game_02.png")

  -- Blits
  liftIO $ do
    applySurface 0 0 newGameBg screen Nothing

  return ()



{-
***********************************************************************
*            MenuSettings Rendering
***********************************************************************
-}
menuSettingsRendering :: AppEnv ()
menuSettingsRendering = do
  problem
  -- parse settings from settings file
  -- load the font in initEnv
  -- create messages with settings
  -- display them.