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
             , Direction(..)
             , GameData(..)
             , Pokemon(..)
             , Player(..)
             , Item(..)
             , Inventory(..)
             , Badge(..)
             , Team(..) 
             ) where

import Control.Monad.Reader
import Control.Monad.State

import Data.Array
import Data.Map (Map)
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
    , resPlayerSprites :: Surface -- the global player sprite sheet
    , resPlayerClips   :: Map Direction Rect -- main player's clips
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
    , appGameData     :: Maybe GameData  -- all info about the player (saved/loaded)
    } deriving (Show)


-- Contains data about the player (is what is saved/loaded)
data GameData = GameData {
      gPlayer          :: Player
    , gPos             :: (Int, Int) -- (x, y) player position on the map (in tiles)
    , gDir             :: Direction  -- to know what tile to display
    , gClock           :: Integer    -- Stores the timestamp when the charatcers started playing.
    } deriving (Show, Read)

-- Pokemon player''s info
data Player = Player {
      plName      :: String
    , plTeam      :: Team
    , plInventory :: Inventory
    , plBadges    :: [Badge]
    } deriving (Show, Read)

-- A Pokemon Team info
data Team = Team {
      t1    :: Pokemon
    , t2    :: Pokemon
    , t3    :: Pokemon
    , t4    :: Pokemon
    , t5    :: Pokemon
    , t6    :: Pokemon
    } deriving (Show, Read)

-- A player's inventory
data Inventory = Inventory {
      iSize     :: Int -- nb of items in inventory
    , iItems    :: [Item]
    } deriving (Show, Read)

-- Representation of an item
-- Not effect attached to items for now, just display
data Item = Item {
      itName :: String
    , itDesc :: String
} deriving (Show, Read)

-- Representation of a badge
data Badge =
             BadgeOne
           | BadgeTwo
           | BadgeThree
           | BadgeFour
           | BadgeFive
           | BadgeSix
           | BadgeSeven
           | BadgeHeight
             deriving (Show, Read, Eq)

-- Representation of a Pokemon
data Pokemon = Pokemon {
      kName       :: String
    , kNumner     :: Int    -- its number in the Pokedex
    , kHp         :: Int    -- current life
    , kMaxHp      :: Int    -- max life
    , kAttack     :: Int
    , kDefense    :: Int
    , kSpAttack   :: Int
    , kSpDefense  :: Int
    , kSpeed      :: Int
    } deriving (Show, Read)

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
                 deriving (Eq, Show, Read, Bounded, Enum, Ord)

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