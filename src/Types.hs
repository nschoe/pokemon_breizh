{--
----------------------------------------------------------------
-     This module simply defines all the custom types
-     used in the map editor.
----------------------------------------------------------------
--}

module Types (
               Pixels
             , Camera
             , Alist
             , AppResource(..)
             , AppData(..)
             , AppState
             , AppEnv
             , GameState(..)
             , Player(..)
             , Direction(..)
             ) where

import Control.Monad.Reader
import Control.Monad.State

import Data.Array
import Data.Word (Word16)

import Graphics.UI.SDL (Rect, Surface)
import Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF.Management
import Graphics.UI.SDL.TTF.Render
import Graphics.UI.SDL.TTF.Types (Font)

import Pokemap
import Timer

{-
*****************************************************************
*                      Type synonyms
*****************************************************************
-}

-- Used to clarify functions signature when they need the dimensions in tiles
type Pixels = Int

type Camera = Rect

type Alist  = Array Int Rect

type AppState = StateT AppData IO

type AppEnv   = ReaderT AppResource AppState

{-
*****************************************************************
*                      End of Type synonyms
*****************************************************************
-}


{-
*****************************************************************
*                      Custom Data Types
*****************************************************************
-}

-- Read-only material
data AppResource = AppResource {
      resScreen        :: Surface -- the screen to blit on
    , resIntroBg       :: Surface -- Introduction screen background
    , resCreditsBg     :: Surface -- Credits screen background
    , resMenuArrow     :: Surface -- The little arrow used to select menu
    , resMenuBg        :: Surface -- menu background, with selection options
    , resSpriteSheet   :: Surface -- the tile sprites
    , resPlayerSprites :: Surface -- main player's sprites
    , resPokemonFont   :: Font    -- Pokemon GB font
    } deriving (Show)

-- State of the application
data AppData = AppData {
      appWorld        :: World     -- the map
    , appFps          :: Timer     -- to cap frame rate
    , appCamera       :: Camera    -- our field of vision
    , appMenuSelector :: (Int,Int) -- (pos, maxValue) of our menu selector
    , appNewGameBgs   :: Maybe (Array Int Surface) -- Set to Nothing when loading game
    , appCurrentState :: GameState -- current state the application is in
    , appNextState    :: GameState -- next state to be set when calling changeState
    , appPlayer       :: Maybe Player
    } deriving (Show)

data Player = Player {
      plName      :: String
    , plPos       :: (Int, Int)
    , plDir       :: Direction
    } deriving (Show)

-- Basic Enum type to know what player's sprite to draw
data Direction =
                 StopUp
               | StopDown
               | StopLeft
               | StopRight
               | WalkingUp
               | WalkingDown
               | WalkingLeft
               | WalkingRight
                 deriving (Eq, Show, Bounded, Enum)

data GameState =
   -- implementation-necessary
   Null

   -- displays the PoKemon Breizh intro screen
 | Intro

   -- displays our names + techno used (Haskell, SDL, ...)
 | Credits

   -- displays new game, continue, ...
 | Menu

   -- First message of Prof. Euclide
 | NewGame01

   -- Second message of Prof. Euclide
 | NewGame02

   -- Edit the global game settings (from the menu)
 | MenuSettings

   -- set when the user wants to exit the game
 | Bye

   -- most common, "normal" game state: character moving in the wild!
 | Exploring

   deriving (Eq, Show)

{-
*****************************************************************
*                      End of Custom Data Type
*****************************************************************
-}