module Rendering (
                  rendering
                  ) where

import Control.Monad (liftM, forM_)
import Control.Monad.IO.Class (liftIO)

import Data.Array ((!))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

import Graphics.UI.SDL (fillRect, getClipRect, surfaceGetPixelFormat, mapRGB, Rect(..))
import Graphics.UI.SDL.Color (Color(..))
import qualified Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF.Management
import Graphics.UI.SDL.TTF.Render

import System.FilePath ((</>))

import Config(clips)
import Helper
import Pokemap(World(..))
import Types
import Settings

-- Main rendering function, makes appropriate calls to rendering functions
rendering :: GameState -> AppEnv ()
rendering Intro               = introRendering
rendering Credits             = creditsRendering
rendering Menu                = menuRendering
rendering NewGame01           = newGame01Rendering
rendering NewGame02           = newGame02Rendering
rendering NewGame03           = newGame03Rendering
rendering MenuSettings        = menuSettingsRendering
rendering (Exploring mapName (x,y)) = exploringRendering mapName (x,y)
rendering _                   = return ()



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



-- Displays the character, with the right sprite, at the right position
drawCharacter :: AppEnv ()
drawCharacter = do
  -- Gets the character's position and direction
  (Just gd)  <- getGameData
  let (x, y) = gPos gd 
      dir    = gDir gd
  
  -- Gets resources
  playerSprites      <- getPlayerSprites
  playerClips        <- getPlayerClips
  (Rect cx cy cw ch) <- getCamera 
  let clip        = Map.lookup dir playerClips
  
  -- Computes the position for blitting
  let xPixel = ttp x - cx
      yPixel = ttp y - cy
  
  -- Blits
  screen <- getScreen
  liftIO $ applySurface xPixel yPixel playerSprites screen clip
  return ()
  
  

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



{-
***********************************************************************
*            NewGame01 Rendering
***********************************************************************
-}
newGame01Rendering :: AppEnv ()
newGame01Rendering = do
  -- Gets resources
  screen          <- getScreen
  Just bgs        <- getNewGameBgs -- safe: bgs are loaded here.
  let newGameBg   = bgs ! 0

  -- Blits
  liftIO $ do
    applySurface 0 0 newGameBg screen Nothing

  return ()



{-
***********************************************************************
*            NewGame02 Rendering
***********************************************************************
-}
newGame02Rendering :: AppEnv ()
newGame02Rendering = do
  -- Don't forget to call free surface when loading the game
  -- Gets resources
  screen          <- getScreen
  Just bgs        <- getNewGameBgs -- safe: bgs are loaded here.
  let newGameBg   = bgs ! 1

  -- Blits
  liftIO $ do
    applySurface 0 0 newGameBg screen Nothing

  return ()



{-
***********************************************************************
*            NewGame03 Rendering
***********************************************************************
-}
newGame03Rendering :: AppEnv ()
newGame03Rendering = do
  -- Don't forget to call free surface when loading the game
  -- Gets resources
  screen          <- getScreen
  Just bgs        <- getNewGameBgs -- safe: bgs are loaded here.
  let newGameBg   = bgs ! 2

  -- Blits
  liftIO $ do
    applySurface 0 0 newGameBg screen Nothing

  return ()



{-
***********************************************************************
*            MenuSettings Rendering
***********************************************************************
-}
menuSettingsRendering :: AppEnv ()
menuSettingsRendering = do
  -- Parses the settings file
  settings        <- liftIO $ parseSettings settings
  
  -- Gets the resources
  font            <- getPokemonFont
  screen          <- getScreen

  -- Reads the settings
  let
      Just playerName = Map.lookup "playerName" settings -- the application will fail if absent
      Just loveName   = Map.lookup "loveName" settings
      Just pokeMap    = Map.lookup "map" settings

  -- Generates the black screen
  bgColor   <- liftIO $ (mapRGB . surfaceGetPixelFormat) screen 0 0 0
  clipRect  <- liftIO $ getClipRect screen

  -- Generates messages
  playerMsg <- liftIO $ renderTextSolid font ("Votre nom d'aventurier: " ++ playerName) (Color 255 255 255)
  loveMsg   <- liftIO $ renderTextSolid font ("Le nom de votre bien aimee: " ++ loveName) (Color 255 255 255)
  mapMsg    <- liftIO $ renderTextSolid font ("Vous jouez sur la carte: " ++ pokeMap) (Color 255 255 255)

  -- Blits
  liftIO $ do
    fillRect screen (Just clipRect) bgColor
    applySurface 75 70 playerMsg screen Nothing
    applySurface 75 130 loveMsg screen Nothing
    applySurface 75 190 mapMsg screen Nothing

  return ()
  
  
  
{-
***********************************************************************
*            Exploring Rendering
***********************************************************************
-}
exploringRendering :: String -> (Int, Int) -> AppEnv ()
exploringRendering mapName (x,y) = do
  -- First: draw the map
  drawMap
  
  -- Then draw the character
  drawCharacter