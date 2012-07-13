module Logic (
              performLogic
             ) where

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

import Data.Map as Map (Map, lookup)
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX

import Graphics.UI.SDL (Rect(..))

import Config (tileDim, startingPosition)
import Helper (settings, getGameData, putGameData, putCamera, getCamera, ttp, getInsideWorld, getCurrentWorld)
import Pokemap (World(..))
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
performLogic (Exploring mapName (x,y)) = exploringLogic mapName (x,y)
performLogic _               = error "logic not handled!"



-- Centers the camera over the character
centerCamera :: AppEnv ()
centerCamera = do
  -- Gets the game data (should always be called when game data is initialized)
  (Just gd)  <- getGameData
  
  -- Gets the position of the player and the current camera
  let (x, y)               = gPos gd
  camera@(Rect cx cy cw ch) <- getCamera
  
  -- Guesses which maps to load, and reads its dimension
  mapIO    <- liftM (gIO . (fromMaybe (error "calling centerCamera where game data not initialized!"))) getGameData
  world@World{ wDim = (lvlW, lvlH) } <- case mapIO of
    Outside -> getCurrentWorld
    Inside  -> liftM (fromMaybe (error "centerCamera called with Inside set and no inside map loaded!")) getInsideWorld

  -- Computes new position
  -- Problem: for now, the field is 16*16, or 16 is even.
  -- So, the character cannot be centered
  let cx'  = ttp x - cw `div` 2
      cy'  = ttp y - ch `div` 2
      cx'' = if (cx'+cw) > (lvlW*tileDim) || cx' < 0 then cx else cx'
      cy'' = if (cy'+ch) > (lvlH*tileDim) || cy' < 0 then cy else cy'
  
  -- Puts the new camera
  putCamera (Rect cx'' cy'' cw ch)



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
      pos         = startingPosition
      dir         = StopDown
      iO          = Outside
      gameData    = GameData {
                      gPlayer = player
                    , gLove   = love
                    , gPos    = pos
                    , gDir    = dir
                    , gClock  = timestamp
                    , gIO     = iO            
                    }
  putGameData (Just gameData)

{-
***********************************************************************
*            MenuSettings Logic
***********************************************************************
-}
menuSettingsLogic :: AppEnv ()
menuSettingsLogic = return ()



{-
***********************************************************************
*            Exploring Logic
***********************************************************************
-}
exploringLogic :: String -> (Int, Int) -> AppEnv ()
exploringLogic mapName (x,y) = do
  -- Centers the camera onto the character
  centerCamera