{-
----------------------------------------------------------------
-     This module provides helper functions to be
-     used accross the project, mostly to save
-     key typing, do conversions, etc.
----------------------------------------------------------------
-}

{-# LANGUAGE FlexibleContexts #-}

module Helper (
                loadImage
              , applySurface
              , root
              , img
              , sound
              , mapDir
              , save
              , fonts
              , settings
              , forbidden  
              , getForbidden  
              , ttp
              , ptt
              , fmn
              , serializeGameData
              , parseGameData
              , saveGame 
              , loadGame  
              , moveCharacter 
              , getScreen
              , getSpriteSheet
              , getIntroBg
              , getCreditsBg
              , getMenuBg
              , getMenuArrow
              , getPokemonFont
              , getSaveFile
              , getPlayerSprites
              , getPlayerClips
              , getCurrentWorld
              , putCurrentWorld
              , getInsideWorld 
              , putInsideWorld
              , getDim
              , getFPS
              , putFPS
              , getCamera
              , putCamera
              , getCurrentState
              , putCurrentState
              , getNextState
              , putNextState
              , getGameData
              , putGameData
              , getMenuSelector
              , putMenuSelector
              , getNewGameBgs
              , putNewGameBgs
              ) where

import Control.Monad (liftM)
import Control.Monad.State
import Control.Monad.Reader

import Data.Array (Array(..), (!))
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map ((!))
import Data.Maybe (fromMaybe)
import Data.Word (Word16)

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image
import Graphics.UI.SDL.TTF.Types (Font)

import System.FilePath (FilePath, (</>))

import Config (tileDim)
import Pokemap (World(..))
import Timer (Timer(..))
import Types



-- Optimize-loads an image (any format), keeping alpha channel
loadImage :: FilePath  -> IO Surface
loadImage filename = load filename >>= displayFormatAlpha



-- Blits a surface, allowing for clipping
applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
    where offset = Just Rect {rectX=x, rectY=y, rectW=0, rectH=0}


          
-- Returns the application's root directory
root :: FilePath
root = ".."



-- Returns the application's image directory
img :: FilePath
img = root </> "img"



-- Returns the application's sound directory
sound :: FilePath
sound = root </> "snd"



-- Returns the application's map directory
mapDir :: FilePath
mapDir = root </> "maps"



-- Returns the game's save directory
save :: FilePath
save = root </> "saves"



-- Returns the application's fonts directory
fonts :: FilePath
fonts = root </> "fonts"



-- Path to settings file
settings :: FilePath
settings = root </> ".settings"



-- Path to the forbidden tile list file
forbidden :: FilePath
forbidden = root </> ".forbidden"



-- Returns the list of all forbidden tiles
getForbidden :: IO [Word16]
getForbidden = do
  -- Reads the file
  rawContents <- liftIO $ readFile forbidden
  
  -- Get the list of Word16
  let separated = words rawContents
      converted = (map read separated) :: [Word16]
  return converted



-- Converts dimension from tiles to pixels
-- TileToPixels
ttp :: Int -> Pixels
ttp n = n * tileDim



-- Converts dimension from pixels to tiles
-- PixelsToTile
ptt :: Pixels -> Int
ptt n = n `div` tileDim



-- Creates full map name (file path) from a map name
-- FullMapName
fmn :: String -> FilePath
fmn mapName = mapDir </> mapName ++ ".pokemap"



-- Serializes a GameData instance, to store in file
serializeGameData :: GameData -> String
serializeGameData = show



-- Reads a save file and returns the associated GameData
parseGameData :: FilePath -> IO GameData
parseGameData saveFile = do
  contents <- readFile saveFile
  return (read contents)


-- Saves the game in the save file
saveGame :: AppEnv ()
saveGame = do
  saveFile <- getSaveFile
  (Just gd) <- getGameData
  liftIO $ writeFile saveFile (serializeGameData gd)
  
  
  
-- Loads the game from the save file  
loadGame :: AppEnv ()
loadGame = do
  -- Gets the save file
  saveFile <- getSaveFile
  
  -- Reads the game data from the file
  gd <- liftIO $ parseGameData saveFile

  -- Attaches the newly-loaded game data in the global state
  putGameData (Just gd)



-- Checks if the character can access the asked tile, and move it
moveCharacter :: MoveDir -> AppEnv ()
moveCharacter moveDir = do
  -- Gets GameData and position and world dimension
  (Just gd)                             <- getGameData
  mapIO                                 <- liftM (gIO . (fromMaybe (error "calling movePlayer while Game Data not set!"))) getGameData
   
  world@World{ wField = field, wDim = (lvlW, lvlH) } <- case mapIO of 
    Outside -> getCurrentWorld
    Inside  -> liftM (fromMaybe (error "movePlayer called, with Inside set and no Inside map loaded")) getInsideWorld
  forbiddenTiles <- liftIO $ getForbidden
  
  let (x, y) = gPos gd
  
  -- Computes new position
      (x', y') = case moveDir of
        MoveUp    -> (x, y-1)
        MoveDown  -> (x, y+1)
        MoveLeft  -> (x-1, y)
        MoveRight -> (x+1, y)
  
  -- Checks if accessed square is authorized
  -- should check on list of forbidden tiles...
      x'' = if x' < 0 || x' > lvlW then x else x'
      y'' = if y' < 0 || y' > lvlH then y else y'
      
      nextTileType = field ! (y'', x'')
      (nextX, nextY) = case nextTileType `elem` forbiddenTiles of
        False -> (x'', y'')
        True  -> (x, y)
  
  -- Actually moves the player
  putGameData (Just gd{ gPos = (nextX, nextY) })  



-- Accessor functions
-- AppResource (MonadReader)
getScreen :: MonadReader AppResource m => m Surface
getScreen = liftM resScreen ask

getSpriteSheet :: MonadReader AppResource m => m Surface
getSpriteSheet = liftM resSpriteSheet ask

getIntroBg :: MonadReader AppResource m => m Surface
getIntroBg = liftM resIntroBg ask

getCreditsBg :: MonadReader AppResource m => m Surface
getCreditsBg = liftM resCreditsBg ask

getMenuBg :: MonadReader AppResource m => m Surface
getMenuBg = liftM resMenuBg ask

getMenuArrow :: MonadReader AppResource m => m Surface
getMenuArrow = liftM resMenuArrow ask

getPokemonFont :: MonadReader AppResource m => m Font
getPokemonFont = liftM resPokemonFont ask

getPlayerSprites :: MonadReader AppResource m => m Surface
getPlayerSprites = liftM resPlayerSprites ask

getPlayerClips :: MonadReader AppResource m => m (Map Direction Rect)
getPlayerClips = liftM resPlayerClips ask

getSaveFile :: MonadReader AppResource m => m FilePath
getSaveFile = liftM resSaveFile ask

-- AppData (MonadState)
getCurrentWorld :: MonadState AppData m => m World
getCurrentWorld = liftM appCurrentWorld get

putCurrentWorld :: MonadState AppData m => World -> m ()
putCurrentWorld w = modify $ \s -> s { appCurrentWorld = w }

getInsideWorld :: MonadState AppData m => m (Maybe World)
getInsideWorld = liftM appInsideWorld get

putInsideWorld :: MonadState AppData m => Maybe World -> m ()
putInsideWorld w = modify $ \s -> s { appInsideWorld = w }

getDim :: MonadState AppData m => m (Int, Int)
getDim = liftM (wDim . appCurrentWorld) get

getFPS :: MonadState AppData m => m Timer
getFPS = liftM appFps get

putFPS :: MonadState AppData m => Timer -> m ()
putFPS t = modify $ \s -> s { appFps = t }

getCamera :: MonadState AppData m => m Camera
getCamera = liftM appCamera get

putCamera :: MonadState AppData m => Camera -> m ()
putCamera c = modify $ \s -> s { appCamera = c }

getCurrentState :: MonadState AppData m => m GameState
getCurrentState = liftM appCurrentState get

putCurrentState :: MonadState AppData m => GameState -> m ()
putCurrentState g = modify $ \s -> s { appCurrentState = g }

getNextState :: MonadState AppData m => m GameState
getNextState = liftM appNextState get

putNextState :: MonadState AppData m => GameState -> m ()
putNextState g = modify $ \s -> s { appNextState = g }

getGameData :: MonadState AppData m => m (Maybe GameData)
getGameData = liftM appGameData get

putGameData :: MonadState AppData m => Maybe GameData -> m ()
putGameData g = modify $ \s -> s { appGameData = g }

getMenuSelector :: MonadState AppData m => m (Int, Int)
getMenuSelector = liftM appMenuSelector get

putMenuSelector :: MonadState AppData m => (Int, Int) -> m ()
putMenuSelector n = modify $ \s -> s { appMenuSelector = n }

getNewGameBgs :: MonadState AppData m => m (Maybe (Array Int Surface))
getNewGameBgs = liftM appNewGameBgs get

putNewGameBgs :: MonadState AppData m => Maybe (Array Int Surface) -> m ()
putNewGameBgs n = modify $ \s -> s { appNewGameBgs = n }