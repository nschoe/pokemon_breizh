module Logic (
              performLogic
             ) where

import Types

-- Main logic function, makes calls to suitable functions to handle logic
performLogic :: GameState -> AppEnv ()
performLogic Intro           = introLogic
performLogic Credits         = creditsLogic
performLogic Menu            = menuLogic
performLogic _               = return ()

{-
***********************************************************************
*            Intro Logic
***********************************************************************
-}
introLogic :: AppEnv ()
introLogic = return ()



{-
***********************************************************************
*            Credits Logic
***********************************************************************
-}
creditsLogic :: AppEnv ()
creditsLogic = return ()



{-
***********************************************************************
*            Menu Logic
***********************************************************************
-}
menuLogic :: AppEnv ()
menuLogic = return ()