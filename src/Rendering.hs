module Rendering (
                  rendering
                  ) where

import Control.Monad.IO.Class (liftIO)

import Types
import Helper

-- Main rendering function, makes appropriate calls to rendering functions
rendering :: GameState -> AppEnv ()
rendering Intro = introRendering
rendering _ = return ()



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