module Arguments where

import System.Environment(getArgs)

import Actions

handleArguments = do
  args <- getArgs
  case args of
    ("add":xs) -> addToDo xs
    ["list"] -> listToDos
    ["help"] -> help
    ("kill":xs) -> killToDo xs
    otherwise -> wrong
