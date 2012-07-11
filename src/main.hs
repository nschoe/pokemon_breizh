module Main where

import Control.Monad (forever, when)
import Control.Monad.Reader
import Control.Monad.State

import Data.Array (listArray)
import Data.Map (Map, fromList)

import Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF.Management
import Graphics.UI.SDL.TTF.Render

import System.Exit (exitSuccess, exitFailure)
import System.FilePath (FilePath, (</>))

import Config
import EventsHandler
import Helper
import Logic
import Pokemap
import Rendering
import Timer
import Types



-- Initializes the game environment and resource.
-- Used to populate the State and Reader monads
initEnv :: FilePath -> IO (AppResource, AppData)
initEnv mapFile = do
  -- names our application's window
  setCaption "PoKemon Breizh .: nschoe" []
  
  -- initializes the reader monad (read-only material)
  screen        <- setVideoMode sWidth sHeight sBpp sFlags
  introBg       <- loadImage (img </> "intro_bg.png")
  creditsBg     <- loadImage (img </> "credits_bg.png")
  menuArrow     <- loadImage (img </> "menu_arrow.png")
  menuBg        <- loadImage (img </> "menu_bg.png")
  spriteSheet   <- loadImage (img </> "sprite_sheet.png")
  playerSprites <- loadImage (img </> "player_sprites.png")
  newGame01     <- loadImage (img </> "new_game_01.png")
  newGame02     <- loadImage (img </> "new_game_02.png")
  newGame03     <- loadImage (img </> "new_game_03.png")

  -- initializes the state monad (read-write material)
  world          <- parseMap (fmn mapFile)
  fps            <- start defaultTimer
  pokemonFont    <- openFont (fonts </> "pokemon_gb.ttf") 20
  let camera     = Rect 0 0 sWidth sHeight
      currentSt  = Intro
      nextSt     = Null
      gameData   = Nothing       -- no data yet (will be loaded from save game)
      arrowPos   = (0, 3)
      newGameBgs = Just (listArray (0,2) [newGame01, newGame02, newGame03])
      plClpsList = map (\(dir, (x,y,w,h)) -> (dir, Rect x y w h)) [
                                                                   (StopUp, (186, 0, 48, 48))
                                                                 , (StopDown, (128, 65, 48, 48))
                                                                 , (StopLeft, (184, 129, 48, 48))
                                                                 , (StopRight, (192, 193, 48, 48))
                                                                 , (WalkingUp, (128, 0, 48, 48))
                                                                 , (WalkingDown, (129, 129, 48, 48))
                                                                 , (WalkingLeft, (579, 316, 48, 48))
                                                                 , (WalkingRight, (633, 316, 48, 48))
                                                                  ]
      plClips    = fromList plClpsList

  -- builds the appResource
      res       = AppResource {
                    resScreen         = screen
                  , resIntroBg        = introBg
                  , resCreditsBg      = creditsBg
                  , resMenuArrow      = menuArrow
                  , resMenuBg         = menuBg
                  , resSpriteSheet    = spriteSheet
                  , resPlayerSprites  = playerSprites
                  , resPlayerClips    = plClips
                  , resPokemonFont    = pokemonFont
                  }
  
  -- builds the appData
      dat       = AppData {
                    appWorld          = world
                  , appFps            = fps
                  , appCamera         = camera
                  , appNewGameBgs     = newGameBgs
                  , appMenuSelector   = arrowPos
                  , appCurrentState   = currentSt
                  , appNextState      = nextSt
                  , appGameData       = gameData
                  }

  -- Finally returns them (this form makes it easier to handle when updating the appResource and appData types)
  return (res, dat)



-- Application's main loop
loop :: AppEnv ()
loop = forever $ do
  currentState <- getCurrentState

  if (currentState == Bye) then
      -- log?
      liftIO exitSuccess
  else do
      
    -- Starts the timer for capping frames
    getFPS >>= liftIO . start >>= putFPS

    -- Handles application events
    handleEvents currentState

    -- Performs the logic computations
    performLogic currentState

    -- Changes state when needed
    changeState

    -- Gets back the new current state (might have just changed)
    updatedCurrentState <- getCurrentState

    -- Renders the screen
    rendering updatedCurrentState

    -- Flips SDL screen
    screen <- getScreen
    liftIO $ SDL.flip screen

    -- Caps the frame rate
    ticks <- getFPS >>= liftIO . getTimerTicks
    when (ticks < secsPerFrame) $ do
         liftIO . SDL.delay $ secsPerFrame - ticks

  



main :: IO ()
main = withInit [InitVideo, InitTimer] $ do
         ttfInit <- TTFG.init
         if not ttfInit then do
             putStrLn "Failed to initialize SDL TTF"
             exitFailure
         else do
           enableKeyRepeat 100 100
           (res, dat) <- initEnv "breizh"
           evalStateT (runReaderT loop res) dat
           TTFG.quit


{- Test Part -}
{-
pokemon = Pokemon "bulbizarre" 1 100 100 15 14 13 12 11
team = Team pokemon pokemon pokemon pokemon pokemon pokemon
inventory = Inventory 2 [(Item "argent" "Sert a payer les objets"), (Item "Super Bonbon" "Augmente d'un le niveau d'un pokemon")]
badges = [BadgeThree, BadgeFour]
player = Player "nicolas" team inventory badges
pos = (33, 42)
dir = StopDown
clock = 1234567890
gameData = GameData player pos dir clock
-}