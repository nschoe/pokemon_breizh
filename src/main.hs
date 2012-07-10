module Main where

import Control.Monad (forever, when)
import Control.Monad.Reader
import Control.Monad.State

import Graphics.UI.SDL as SDL

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
  playerSprites <- loadImage (img </> "player.png")

  -- initializes the state monad (read-write material)
  world         <- parseMap (fmn mapFile)
  fps           <- start defaultTimer
  let camera    = Rect 0 0 sWidth sHeight
      currentSt = Intro
      nextSt    = Null
      player    = Nothing       -- no player yet (still intro screen)
      arrowPos  = (0, 3)

  -- builds the appResource
      res       = AppResource {
                    resScreen         = screen
                  , resIntroBg        = introBg
                  , resCreditsBg      = creditsBg
                  , resMenuArrow      = menuArrow
                  , resMenuBg         = menuBg
                  , resSpriteSheet    = spriteSheet
                  , resPlayerSprites  = playerSprites
                  }
  
  -- builds the appData
      dat       = AppData {
                    appWorld          = world
                  , appFps            = fps
                  , appCamera         = camera
                  , appMenuSelector   = arrowPos
                  , appCurrentState   = currentSt
                  , appNextState      = nextSt
                  , appPlayer         = player
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
         enableKeyRepeat 100 100
         (res, dat) <- initEnv "breizh"
         evalStateT (runReaderT loop res) dat