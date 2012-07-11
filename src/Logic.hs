module Logic (
              performLogic
             ) where

import Control.Monad.IO.Class (liftIO)

import Data.Map as Map (Map, lookup)
import Data.Time.Clock.POSIX

import Helper (settings, putGameData)
import Settings
import Types

-- Main logic function, makes calls to suitable functions to handle logic
performLogic :: GameState -> AppEnv ()
performLogic Intro           = introLogic
performLogic Credits         = creditsLogic
performLogic Menu            = menuLogic
performLogic NewGame01       = newGame01Logic
performLogic NewGame02       = newGame02Logic
performLogic NewGame03       = newGame03Logic
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
*            NewGame03 Logic
***********************************************************************
-}
newGame03Logic :: AppEnv ()
newGame03Logic = do
  -- Parse the settings file, to get the name and the map
  sets        <- liftIO $ parseSettings settings

  time        <- liftIO $ getPOSIXTime

  -- Creates the game data/player/team/...
  let Just pName  = Map.lookup "playerName" sets
      Just love   = Map.lookup "loveName" sets
      Just m      = Map.lookup "map" sets
      player      = Player {
                      plName = pName
                    , plTeam = Nothing
                    , plInventory = Inventory {iSize = 1, iItems = [Item "Argent" "Sert a acheter les objets utiles a vos PoKemon !"]}
                    , plBadges = []
                    }

      timestamp   = read (takeWhile ((/=) '.') (show time)) :: Integer
      pos         = (1, 2)
      dir         = StopDown
      gameData    = GameData {
                      gPlayer = player
                    , gLove   = love
                    , gPos    = pos
                    , gDir    = dir
                    , gClock  = timestamp
                    }
  putGameData (Just gameData)

{-
***********************************************************************
*            MenuSettings Logic
***********************************************************************
-}
menuSettingsLogic :: AppEnv ()
menuSettingsLogic = return ()