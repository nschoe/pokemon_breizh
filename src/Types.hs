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
    , resSpriteSheet   :: Surface -- the tile sprites
    , resPlayerSprites :: Surface -- main player's sprites
    } deriving (Show)

-- State of the application
data AppData = AppData {
      appWorld        :: World     -- the map
    , appFps          :: Timer     -- to cap frame rate
    , appCamera       :: Camera    -- our field of vision
    , appCurrentState :: GameState -- current state the application is in
    , appNextState    :: GameState -- next state to be set when calling changeState
    , appPlayer       :: Maybe Player
    } deriving (Show)

data Player = Player {
      plName      :: String
    , plPos       :: (Int, Int)
    , plDir       :: Direction
    } deriving (Show)

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