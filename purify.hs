import Data.Word (Word16)
import Data.List  (nub)


main :: IO ()
main = do
  rawContents <- readFile (".forbidden")
  let separated = words rawContents
      nubbed    = nub separated
      pured     = unwords nubbed
  putStrLn pured
  writeFile ".forbidden" pured