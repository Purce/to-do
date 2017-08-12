module Main where

import System.Console.GetOpt
import System.Directory (getHomeDirectory, doesFileExist)
import System.Environment (getArgs)

import ToDo.Options

main :: IO ()
main = do
  
  args <- getArgs
  let (actions, nonOptions, errors) = getOpt RequireOrder options args

  home <- getHomeDirectory
  check <- doesFileExist (home ++ "/.todo")
  if check
    then return ()
    else writeFile (home ++ "/.todo") ""
    
  opts <- foldl (>>=) (return startOptions) actions
  putStrLn "Not a valid command.\n-h is all you need"
