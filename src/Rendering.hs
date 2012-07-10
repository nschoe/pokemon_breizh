module Rendering (
                  rendering
                  ) where

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

import Types
import Helper

-- Main rendering function, makes appropriate calls to rendering functions
rendering :: GameState -> AppEnv ()
rendering Intro               = introRendering
rendering Credits             = creditsRendering
rendering Menu                = menuRendering
rendering _                   = return ()



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