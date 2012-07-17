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
              , centerCamera  
              , drawMap  
              , moveCharacter 
              , getScreen
              , getSpriteSheet
              , getIntroBg
              , getCreditsBg
              , getMenuBg
              , getMenuArrow
              , getInGameMenuSprite  
              , getInGameArrowSprite  
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
import qualified Data.Map as Map ((!), lookup)
import Data.Maybe (fromMaybe)
import Data.Word (Word16)

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image
import Graphics.UI.SDL.TTF.Types (Font)

import System.FilePath (FilePath, (</>))

import Config (tileDim, clips)
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
  
  
  
-- Function to display the map on the screen
-- drawMap assumes the Game Data is filled in (Nothing will fail)
drawMap :: AppEnv ()
drawMap = do
  -- Knows if we have to draw the inside or current map
  mapIO <- liftM (gIO . (fromMaybe (error "calling drawMap while Game Data not set!"))) getGameData
   
  world@World{ wField = field, wDim = (width, height) } <- case mapIO of 
    Outside -> getCurrentWorld
    Inside  -> liftM (fromMaybe (error "drawMap called, with Inside set and no Inside map loaded")) getInsideWorld
    
  -- Gets other resources
  camera@(Rect cx cy cw ch) <- getCamera
  screen                    <- getScreen
  spriteSheet               <- getSpriteSheet
  
  -- Creates a list of the visible tiles
  let topLeftX     = ptt cx
      topLeftY     = ptt cy
      bottomRightX = ptt (cx + cw)
      bottomRightY = ptt (cy + ch)
      visibleTiles = [(i,j) | i <- [topLeftX..bottomRightX-1], j <- [topLeftY..bottomRightY-1]]


  -- Blits them
  liftIO $ forM_ visibleTiles $ \(i,j) -> do
    let (x,y) = (ttp i - cx, ttp j - cy)
        clip  = clips ! (field ! (j,i))
    applySurface x y spriteSheet screen (Just clip)  
    
    
    
-- Centers the camera over the character
centerCamera :: (Int, Int) -> AppEnv ()
centerCamera (x, y) = do  
  -- Gets the position of the player and the current camera
  camera@(Rect cx cy cw ch) <- getCamera
  
  -- Guesses which maps to load, and reads its dimension
  mapIO    <- liftM (gIO . (fromMaybe (error "calling centerCamera where game data not initialized!"))) getGameData
  world@World{ wDim = (lvlW, lvlH) } <- case mapIO of
    Outside -> getCurrentWorld
    Inside  -> liftM (fromMaybe (error "centerCamera called with Inside set and no inside map loaded!")) getInsideWorld

  -- Computes new position
  -- Problem: for now, the field is 16*16, or 16 is even.
  -- So, the character cannot be centered
  let cx'  = x - cw `div` 2
      cy'  = y - ch `div` 2
      cx'' = if (cx'+cw) > (lvlW*tileDim) || cx' < 0 then cx else cx'
      cy'' = if (cy'+ch) > (lvlH*tileDim) || cy' < 0 then cy else cy'
  
  -- Puts the new camera
  putCamera (Rect cx'' cy'' cw ch)



-- Checks if the character can access the asked tile, and moves it
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
        MoveUp    -> (x, y-tileDim)
        MoveDown  -> (x, y+tileDim)
        MoveLeft  -> (x-tileDim, y)
        MoveRight -> (x+tileDim, y)
        
  
  -- Keeps next position in bound
      x'' = if x' < 0 || x' > (lvlW * tileDim) then x else x'
      y'' = if y' < 0 || y' > (lvlH * tileDim) then y else y'
      
      nextTileType = field ! (ptt y'', ptt x'')
      
  -- Proceeds to the mouvement if next tile is valid
  when (not (nextTileType `elem` forbiddenTiles)) (animatedWalking moveDir)
  
  
  
-- Animates the walking from a tile to an adjacent one  
animatedWalking :: MoveDir -> AppEnv ()
animatedWalking moveDir = do
  -- Gets resources
  screen           <- getScreen
  (Rect cx cy _ _) <- getCamera
  playerSprites    <- getPlayerSprites
  playerClips      <- getPlayerClips
  
  -- Gets the current player's position
  (Just gd)       <- getGameData
  let (x, y)       = gPos gd
  
  -- Creates the list of intermediate positions based on the MoveDir
      positions    = case moveDir of
        MoveUp     -> [(x,toY) | toY <- [y,y-4..y-tileDim]]
        MoveDown   -> [(x,toY) | toY <- [y,y+4..y+tileDim]]
        MoveLeft   -> [(toX,y) | toX <- [x,x-4..x-tileDim]]
        MoveRight  -> [(toX,y) | toX <- [x,x+4..x+tileDim]]
  
  -- Shifts position with respect to the camera
      shiftedPositions = map (\(x', y') -> (x'-cx,y'-cy)) positions
  
      spriteDir    = case moveDir of
          MoveUp     -> if even (ptt x + ptt y) then WalkingUp else WalkingUp'
          MoveDown   -> if even (ptt x + ptt y) then WalkingDown else WalkingDown'
          MoveLeft   -> if even (ptt x + ptt y) then WalkingLeft else WalkingLeft'
          MoveRight  -> if even (ptt x + ptt y) then WalkingRight else WalkingRight'  
      
      clip         = Map.lookup spriteDir playerClips
  forM_ positions $ \(pX, pY) -> do
    centerCamera (pX, pY)
    (Rect cx cy   _ _) <- getCamera
    drawMap
    liftIO $ applySurface (pX-cx) (pY-cy) playerSprites screen clip
    liftIO $ SDL.flip screen
    liftIO $ SDL.delay 10

  -- Updates positions
  putGameData (Just gd{ gPos = (last positions) })
  
-- Animates the walking process from the start position to the target position
  {-
animatedWalking :: (Int, Int) -> (Int,  Int) -> MoveDir -> AppEnv ()
animatedWalking (fromX, fromY) (toX, toY) dir = do
  -- Computes new positions (to intermediate displaying)
  let positions = case dir of
        MoveUp    -> [(fromX, y) | y <- [fromY,fromY-1..toY]]
        MoveDown  -> [(fromX, y) | y <- [fromY..toY]]
        MoveLeft  -> [(x, fromY) | x <- [fromX,fromX-1..toX]]
        MoveRight -> [(x, fromY) | x <- [fromX..toX]]
        
  -- Gets resources
  playerSprites     <- getPlayerSprites
  playerClips       <- getPlayerClips
  (Just gd)         <- getGameData
  screen            <- getScreen
  (Rect cx cy _ _)  <- getCamera
  let spriteDir     = gDir gd
      clip          = Map.lookup spriteDir playerClips
      displayPos    = map (\(x,y) -> (x-cx, y-cy)) positions
        
  -- Blits all intermediate positions
  forM_ displayPos $ \(x,y) -> do
    centerCamera (x,y) 
    drawMap
    liftIO $ applySurface x y playerSprites screen clip
    liftIO $ SDL.flip screen
    liftIO $ SDL.delay 5
  
  -- Updates final position
  putGameData (Just gd{ gPos = (toX, toY) })
-}

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

getInGameMenuSprite :: MonadReader AppResource m => m Surface
getInGameMenuSprite = liftM resInGameMenuSprite ask

getInGameArrowSprite :: MonadReader AppResource m => m Surface
getInGameArrowSprite = liftM resInGameArrowSprite ask

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