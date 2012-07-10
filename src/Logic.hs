module Logic (
              performLogic
             ) where

import Types

-- Main logic function, makes calls to suitable functions to handle logic
performLogic :: GameState -> AppEnv ()
performLogic Intro = introLogic
performLogic _     = return ()

{-
***********************************************************************
*            Intro Logic
***********************************************************************
-}

introLogic :: AppEnv ()
introLogic = return ()