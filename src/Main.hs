module Main where

import System.Directory (getHomeDirectory,
                         doesFileExist)

import Arguments

main :: IO ()
main = do

  home <- getHomeDirectory
  check <- doesFileExist (home ++ "/.todo")
  if check
    then return ()
    else writeFile (home ++ "/.todo") ""
  
  handleArguments
