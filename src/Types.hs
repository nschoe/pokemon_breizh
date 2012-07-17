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
             , Where(..)
             , MoveDir(..) 
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
    , resInGameMenuSprite :: Surface -- The in game menu  
    , resInGameArrowSprite :: Surface -- the in game arrow  
    , resCreditsBg     :: Surface -- Credits screen background
    , resMenuArrow     :: Surface -- The little arrow used to select menu
    , resMenuBg        :: Surface -- menu background, with selection options
    , resSpriteSheet   :: Surface -- the tile sprites
    , resPlayerSprites :: Surface -- the global player sprite sheet
    , resPlayerClips   :: Map Direction Rect -- main player's clips
    , resPokemonFont   :: Font    -- Pokemon GB font
    , resSaveFile      :: FilePath -- file which contains the save game 
    } deriving (Show)

-- State of the application
data AppData = AppData {
      appCurrentWorld :: World     -- the world we play on
    , appInsideWorld  :: Maybe World -- stores the map on the inside of a building 
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
    , gLove            :: String
    , gPos             :: (Int, Int) -- (x, y) player position on the map (in pixel)
    , gDir             :: Direction  -- to know what tile to display
    , gClock           :: Integer    -- Stores the timestamp when the charatcers started playing.
    , gIO              :: Where      -- Tells if we must display the Inside or Outside world 
    } deriving (Show, Read)
               
-- Type to tell the drawing function to draw either the inside or outside map
data Where = Inside | Outside
           deriving   (Eq, Show, Read)

-- Pokemon player''s info
data Player = Player {
      plName      :: String
    , plTeam      :: Maybe Team
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
    , itQty  :: Int
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
    , kNumber     :: Int    -- its number in the Pokedex
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
               | WalkingUp'
               | WalkingDown
               | WalkingDown'
               | WalkingLeft
               | WalkingLeft'
               | WalkingRight
               | WalkingRight'
                 deriving (Eq, Show, Read, Bounded, Enum, Ord)
                          
-- Basic Enum type used in moveCharacter, to know what direction the player wants to go
data MoveDir = MoveUp | MoveDown | MoveLeft | MoveRight
             deriving (Eq, Show, Read)

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

   -- Third message of Prof. Euclide (Init game data)
 | NewGame03

   -- Edit the global game settings (from the menu)
 | MenuSettings

   -- set when the user wants to exit the game
 | Bye

   -- most common, "normal" game state: character moving in the wild!
 | Exploring Where (Int, Int) -- map and (x,y) position (in pixels)
   
   -- shows the menu when in game
 | InGameMenu

   deriving (Eq, Show)

{-
*****************************************************************
*                      End of Custom Data Type
*****************************************************************
-}