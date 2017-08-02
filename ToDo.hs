module ToDo where

import System.Environment
import System.Directory
import Data.Char

main :: IO ()
main = do
  (file : _) <- getArgs
  
  home <- getHomeDirectory
  createDirectory "ok"
  writeFile (home ++ "/" ++ file) "eccoci"
  
  let ok = Data.Char.isSpace 'c'
  createDirectory "aaa"
  
