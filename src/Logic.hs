module Logic (
              performLogic
             ) where

import Types

-- Main logic function, makes calls to suitable functions to handle logic
performLogic :: GameState -> AppEnv ()
performLogic Intro           = introLogic
performLogic Credits         = creditsLogic
performLogic Menu            = menuLogic
performLogic NewGame01       = newGame01Logic
performLogic NewGame02       = newGame02Logic
performLogic MenuSettings    = menuSettingsLogic
performLogic _               = error "logic not handled!"

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



{-
***********************************************************************
*            NewGame01 Logic
***********************************************************************
-}
newGame01Logic :: AppEnv ()
newGame01Logic = return ()



{-
***********************************************************************
*            NewGame02 Logic
***********************************************************************
-}
newGame02Logic :: AppEnv ()
newGame02Logic = return ()



{-
***********************************************************************
*            MenuSettings Logic
***********************************************************************
-}
menuSettingsLogic :: AppEnv ()
menuSettingsLogic = return ()