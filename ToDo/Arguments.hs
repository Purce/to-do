module ToDo.Arguments where

import System.Environment(getArgs)

import ToDo.Actions


handleArguments = do
  args <- getArgs
  case args of
    ["add", a] -> addToDo a
    ["list"] -> listToDos
    ["help"] -> help
    otherwise -> wrong
