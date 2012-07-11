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
              , ttp
              , ptt
              , fmn
              , getScreen
              , getSpriteSheet
              , getIntroBg
              , getCreditsBg
              , getMenuBg
              , getMenuArrow
              , getPokemonFont
              , getWorld
              , putWorld
              , getDim
              , getFPS
              , putFPS
              , getCamera
              , putCamera
              , getCurrentState
              , putCurrentState
              , getNextState
              , putNextState
              , getPlayer
              , putPlayer
              , getMenuSelector
              , putMenuSelector
              , getNewGameBgs
              , putNewGameBgs
              ) where

import Control.Monad (liftM)
import Control.Monad.State
import Control.Monad.Reader

import Data.Array (Array(..), (!))
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



-- Accessor functions
-- AppResource (MonadReader)
getScreen :: MonadReader AppResource m => m Surface
getScreen = liftM resScreen ask

getSpriteSheet :: MonadReader AppResource m => m Surface
getSpriteSheet = liftM resSpriteSheet ask

getPlayerSprites :: MonadReader AppResource m => m Surface
getPlayerSprites = liftM resPlayerSprites ask

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

-- AppData (MonadState)
getWorld :: MonadState AppData m => m World
getWorld = liftM appWorld get

putWorld :: MonadState AppData m => World -> m ()
putWorld w = modify $ \s -> s { appWorld = w }

getDim :: MonadState AppData m => m (Int, Int)
getDim = liftM (wDim . appWorld) get

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

getPlayer :: MonadState AppData m => m (Maybe Player)
getPlayer = liftM appPlayer get

putPlayer :: MonadState AppData m => (Maybe Player) -> m ()
putPlayer p = modify $ \s -> s { appPlayer = p }

getMenuSelector :: MonadState AppData m => m (Int, Int)
getMenuSelector = liftM appMenuSelector get

putMenuSelector :: MonadState AppData m => (Int, Int) -> m ()
putMenuSelector n = modify $ \s -> s { appMenuSelector = n }

getNewGameBgs :: MonadState AppData m => m (Maybe (Array Int Surface))
getNewGameBgs = liftM appNewGameBgs get

putNewGameBgs :: MonadState AppData m => Maybe (Array Int Surface) -> m ()
putNewGameBgs n = modify $ \s -> s { appNewGameBgs = n }