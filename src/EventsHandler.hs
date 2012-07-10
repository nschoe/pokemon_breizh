module EventsHandler (
                       handleEvents
                     , changeState
                     ) where

import Control.Monad (when, liftM)
import Control.Monad.IO.Class (liftIO)

import Graphics.UI.SDL as SDL

import Helper
import Types


-- Main events handling function, called by main.
-- Calls appropriate events handling function based on the GameState
handleEvents :: GameState -> AppEnv ()
handleEvents gs = do
  event <- liftIO pollEvent
  case event of
    Quit                                           -> setNextState Bye
    KeyDown (Keysym SDLK_q [KeyModLeftCtrl] _)     -> setNextState Bye
    KeyDown (Keysym SDLK_f [KeyModLeftAlt] _)      -> getScreen >>= liftIO . toggleFullscreen
    NoEvent                                        -> return ()
    _                                              -> do
                    handleEvents' gs event
                    handleEvents gs



-- Makes appropriate function calls to handle events, based on GameState
handleEvents' :: GameState -> Event -> AppEnv ()
handleEvents' Intro event           = introEvents event
handleEvents' Credits event         = creditsEvents event
handleEvents' Menu event            = menuEvents event
--handleEvents' _ _                   = error "events not detected"



-- Updates the contents of appNextState, provided it is not already set to Bye
setNextState :: GameState -> AppEnv ()
setNextState gs = do
  -- Checks if the user has not already asked for exit
  currentNextState <- getNextState
  when (currentNextState /= Bye) (putNextState gs)



-- Controls whether we actually need to switch states, and calls the switching function
changeState :: AppEnv ()
changeState = do
  -- Gets the next state, to see if we actually have to switch states
  nextState <- getNextState

  when (nextState /= Null) $ do
     changeState' nextState
     putNextState Null



-- Actual state changing function
changeState' :: GameState -> AppEnv ()

changeState' Bye = putCurrentState Bye
changeState' gs   = putCurrentState gs



{-
***********************************************************************
*            Intro Events
***********************************************************************
-}
introEvents :: Event -> AppEnv ()
introEvents (KeyDown (Keysym SDLK_RETURN [] _)) = setNextState Credits
introEvents _ = return ()

{-
***********************************************************************
*            Credits Events
***********************************************************************
-}
creditsEvents :: Event -> AppEnv ()
creditsEvents (KeyDown (Keysym SDLK_RETURN [] _)) = setNextState Menu
creditsEvents _ = return ()

{-
***********************************************************************
*            Menu Events
***********************************************************************
-}
menuEvents :: Event -> AppEnv ()

-- arrow up
menuEvents (KeyDown (Keysym SDLK_UP [] _)) = do
  (pos,maxi) <- getMenuSelector
  let pos' = if pos == 0 then
                 maxi
             else
                 pred pos
  putMenuSelector (pos', maxi)

-- arrow down
menuEvents (KeyDown (Keysym SDLK_DOWN [] _)) = do
  (pos,maxi) <- getMenuSelector
  let pos' = if pos == maxi then
                 0
             else
                 succ pos
  putMenuSelector (pos', maxi)

-- return key pressed
menuEvents (KeyDown (Keysym SDLK_RETURN [] _)) = do
  pos <- liftM fst getMenuSelector
  when (pos == 3) (setNextState Bye)

menuEvents _ = return ()
