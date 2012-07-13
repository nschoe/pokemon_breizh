module Settings (
  parseSettings
  ) where

import Control.Monad (liftM)

import Data.Char (isSpace)
import Data.Map (Map)
import qualified Data.Map as Map (fromList)

import System.Environment (getArgs)

-- Reads a settings file and returns a map of settings.
parseSettings :: FilePath -> IO (Map String String)
parseSettings fileName = do
  linedContents <- liftM lines $ readFile fileName
  let commentFree = filter isNotComment linedContents
      extracted   = map extractOne commentFree
      setgs       = Map.fromList extracted
  return setgs
      
-- Predicates of a comment line (empty  or begins with '#')
isNotComment :: String -> Bool
isNotComment str = case (dropWhile isSpace str) of
  []   -> False
  (x:_) -> not (x == '#')

-- Returns a (key, value) pair from a string
extractOne :: String -> (String, String)
extractOne str =
  let spacesFree = dropWhile isSpace str
      (key, rest) = break ((==) ']') (tail spacesFree) -- tail removes '['
      spacesFree' = dropWhile isSpace $ tail (dropWhile isSpace (tail rest))
      (value, _)  = break ((==) '>') (tail spacesFree') -- tail removes '<'
  in (key, value)