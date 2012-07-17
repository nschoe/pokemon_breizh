module EventsHandler (
                       handleEvents
                     , changeState
                     ) where

import Control.Monad (when, liftM)
import Control.Monad.IO.Class (liftIO)

import Graphics.UI.SDL as SDL

import System.Directory (doesFileExist)
import System.IO (openFile, IOMode(..), hFileSize)

import Config (startingPosition)
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
handleEvents' NewGame01 event       = newGame01Events event
handleEvents' NewGame02 event       = newGame02Events event
handleEvents' NewGame03 event       = newGame03Events event
handleEvents' (Exploring mapIO (x,y)) event = exploringEvents mapIO (x,y) event
handleEvents' MenuSettings event    = menuSettingsEvents event
handleEvents' InGameMenu event      = inGameMenuEvents event
handleEvents' _ _                   = error "events not detected"



-- Updates the contents of appNextState, provided it is not already set to Bye
setNextState :: GameState -> AppEnv ()
setNextState gs = do
  -- Checks if the user has not already asked for exit
  currentNextState <- getNextState
  when (currentNextState /= Bye) (putNextState gs)
  

{-
-- Actual function to set state (called after verification of wanna quit)
setNextState' :: GameState -> AppEnv ()
setNextState' gs@(Exploring io (x,y)) = do
  
setNextState' gs@(Exploring mapName (x, y)) = do
  -- Sets the gIO accordingly
  -- map name is hardcoded -> to FIX (possibility of several maps: JOHTO, KANTO...)
  let io    = if (mapName == "breizh") then Outside else Inside
  (Just gd) <- getGameData
  putGameData (Just gd{ gIO = io, gPos = (x,y) })
  centerCamera (x,y)
  putNextState gs
-}
setNextState' gs = putNextState gs



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
changeState' gs@(Exploring io (x,y)) = do
  -- Gets the gama data
  (Just gd)    <- getGameData
  
  -- Updates the game data
  putGameData (Just gd{ gIO = io, gPos = (x,y) })
  
  -- Actually changes the state
  putCurrentState gs
  
  -- Centers the camera on the new position we've just went
  centerCamera (x,y)
  
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
  -- Gets the arrow position
  pos <- liftM fst getMenuSelector
  
  -- When pointing to Continue
  when (pos == 0) $ do
    -- Checks if the save file exists and is not empty
    saveFile <- getSaveFile
    exists   <- liftIO $ doesFileExist saveFile 
    size     <- case exists of
      False -> return 0
      True  -> do
        h <- liftIO $ openFile saveFile ReadMode
        liftIO $ hFileSize h
    
    
    -- Actually loads the save game when it (seems to) exist(s)
    let proceed = size > 0
    when proceed $ do
      -- Adds the saved game data in the global state of our program
      loadGame
      
      -- Gets the newly-loaded game data
      (Just gd) <- getGameData
      
      -- Gets map and position form the newly-loaded game data to pass on
      -- as arguments to the Exploring state
      let (x,y) = gPos gd
          io    = gIO gd
    
      -- Starts the game
      setNextState (Exploring io (x, y))
      
    
  -- When pointing to NewGame
  when (pos == 1) (setNextState NewGame01)

  -- Whne pointing to Settings
  when (pos == 2) (setNextState MenuSettings)
       
  -- When pointing to Quit
  when (pos == 3) (setNextState Bye)

menuEvents _ = return ()




{-
***********************************************************************
*            NewGame01 Events
***********************************************************************
-}
newGame01Events :: Event -> AppEnv ()
newGame01Events (KeyDown (Keysym SDLK_RETURN [] _)) = setNextState NewGame02
newGame01Events _ = return ()



{-
***********************************************************************
*            NewGame02 Events
***********************************************************************
-}
newGame02Events :: Event -> AppEnv ()
newGame02Events (KeyDown (Keysym SDLK_RETURN [] _)) = setNextState NewGame03
newGame02Events _ = return ()



{-
***********************************************************************
*            NewGame03 Events
***********************************************************************
-}
newGame03Events :: Event -> AppEnv ()
newGame03Events (KeyDown (Keysym SDLK_RETURN [] _)) = setNextState (Exploring Outside startingPosition)
newGame03Events _ = return ()



{-
***********************************************************************
*            MenuSettings Events
***********************************************************************
-}
menuSettingsEvents :: Event -> AppEnv ()
menuSettingsEvents (KeyDown (Keysym SDLK_ESCAPE [] _)) = setNextState Menu
menuSettingsEvents _ = return ()



{-
***********************************************************************
*            Exploring Events
***********************************************************************
-}
exploringEvents :: Where -> (Int, Int) -> Event -> AppEnv ()

--  Moves character up
exploringEvents mapName (x, y) (KeyDown (Keysym SDLK_UP [] _)) = do
  -- Moves the character up
  moveCharacter MoveUp
  
  -- Sets the new sprite
  (Just gd) <- getGameData
  putGameData (Just gd{ gDir = StopUp })

--  Moves character down
exploringEvents mapName (x, y) (KeyDown (Keysym SDLK_DOWN [] _)) = do
  -- Moves the character down
  moveCharacter MoveDown
  
  -- Sets the new sprite
  (Just gd) <- getGameData
  putGameData (Just gd{ gDir = StopDown })  
  
--  Moves character left
exploringEvents mapName (x, y) (KeyDown (Keysym SDLK_LEFT [] _)) = do
  -- Moves the character left
  moveCharacter MoveLeft
  
  -- Sets the new sprite
  (Just gd) <- getGameData
  putGameData (Just gd{ gDir = StopLeft })  
    
--  Moves character right
exploringEvents mapName (x, y) (KeyDown (Keysym SDLK_RIGHT [] _)) = do
  -- Moves the character right
  moveCharacter MoveRight
  
  -- Sets the new sprite
  (Just gd) <- getGameData
  putGameData (Just gd{ gDir = StopRight })  

-- Start press: displays in game menu
exploringEvents _ _ (KeyDown (Keysym SDLK_RETURN [] _)) = do
  (pos, _) <- getMenuSelector
  putMenuSelector (pos, 6)
  setNextState InGameMenu

exploringEvents _ _ _ = return ()



{-
***********************************************************************
*            In game menu Events
***********************************************************************
-}
inGameMenuEvents :: Event -> AppEnv ()
-- Attention: menu only works OUTSIDE

-- Pops in/out the in-game menu
inGameMenuEvents (KeyDown (Keysym SDLK_RETURN [] _)) = do
  (Just gd) <- getGameData
  let (x,y) = gPos gd
      io    = gIO gd
  setNextState (Exploring io (x,y))
inGameMenuEvents (KeyDown (Keysym SDLK_x [] _)) = do
  (Just gd) <- getGameData
  let (x,y) = gPos gd
      io    = gIO gd
  setNextState (Exploring io (x,y))
  
-- Moving the arrow UP
inGameMenuEvents (KeyDown (Keysym SDLK_UP [] _)) = do
  (pos, maxi) <- getMenuSelector
  let pos' = if pos == 0 then
               maxi
             else  
               pred pos
  putMenuSelector (pos', maxi)

-- Moving the arrow DOWN  
inGameMenuEvents (KeyDown (Keysym SDLK_DOWN [] _)) = do
  (pos, maxi) <- getMenuSelector
  let pos' = if pos == maxi then        
               0 
             else
               succ pos
  putMenuSelector (pos', maxi)  

-- Handles C press
inGameMenuEvents (KeyDown (Keysym SDLK_c [] _)) = do
  (pos, _)  <- getMenuSelector
  (Just gd) <- getGameData
  let (x,y)   = gPos gd
      io      = gIO gd
  
  -- Saves game
  when (pos == 5) $ do
    saveGame
    setNextState (Exploring io (x,y))
  
  -- Quits game
  when (pos == 6) $ do 
    putMenuSelector (0, 3)
    setNextState Menu
inGameMenuEvents _ = return ()