PoKemon Breizh
==============

PoKemon-like Haskell implemented game, base on SDL.

TODO :
  * Move the two World variables to the GameData instance (this way, the World doesn't get loaded in the menu...)
  * In EventsHandler.hs, fix teh hard-coded map name, in function setNextState' (on pattern matching Exploring)
  * Create getters on GameData instead of using the pattern matching functions (for instance, create a getPos, rather than using gPos on a GameData)
  * Revisit code of parseGameData, serializeGameData, loadGame and saveGame, because it is 1) crap, 2) redundant.