module Actions( addToDo
              , help
              , killToDo
              , listToDos
              , wrong
              ) where

import Control.DeepSeq (force)
import Control.Exception.Base (evaluate)
import Data.List (isInfixOf, delete)
import Data.List.Split (splitOn)
import Data.Maybe
import System.Directory (getHomeDirectory)

import Types

addToDo :: [String] -> IO ()
addToDo (name:date:empty) = do
  home <- getHomeDirectory
  contents <- readFile (home ++ "/.todo")
  evaluate (force contents)
  writeSingleToDo (ToDoDate name (Just date))
  putStrLn "A todo has joined our world"
addToDo (name:empty) = do
  home <- getHomeDirectory
  contents <- readFile (home ++ "/.todo")
  evaluate (force contents)
  writeSingleToDo (ToDoDate name Nothing)
  putStrLn "A to-do has joined our world"
addToDo _ = wrong

listToDos :: IO ()
listToDos = do
  putStrLn "To-dos\n"
  home <- getHomeDirectory
  contents <- readFile (home ++ "/.todo")
  let todos = readAllToDos (lines contents)
  printToDos todos

wrong :: IO ()
wrong = putStrLn "Not a valid command. Check \"todo help\""

-- I know, this is bad
help :: IO ()
help = do
  putStrLn "todo - A todo tracker\n"
  putStrLn "Usage:"
  putStrLn "todo [add <name> [<date>]] [kill <number> [name <word>]] [list]\n"
  putStrLn "add          insert a todo in the list"
  putStrLn "list         show the list"
  putStrLn "help         show this message"


killToDo :: [String] -> IO ()
killToDo [number] = killToDoNumber (read number)
killToDo ["name", name] = killToDoName name

killToDoNumber :: Int -> IO ()
killToDoNumber number = do
  home <- getHomeDirectory
  contents <- readFile (home ++ "/.todo")
  let todos = readAllToDos (lines contents)
  let newTodos = delete (todos !! number) todos
  printToDos newTodos
  writeFile (home ++ "/.todo") ""
  rewrite newTodos

findToDosName :: String -> [ToDo] -> [ToDo]
findToDosName _ [] = []
findToDosName string ((ToDoDate name date):xs) = do
  if string `isInfixOf` name
    then (ToDoDate name date) : findToDosName string xs
    else findToDosName string xs

killToDoName :: String -> IO ()
killToDoName string = do
   home <- getHomeDirectory
   contents <- readFile (home ++ "/.todo")
   evaluate (force contents)
   let todos = readAllToDos (lines contents)
   let target = findToDosName string todos
   if length target > 1
     then do
     putStrLn "Many to-dos contains that word"
     printToDosWord string todos 0
     else do
      let todos = readAllToDos (lines contents)
      let newTodos = delete (target !! 0) todos
      writeFile (home ++ "/.todo") ""
      rewrite newTodos
      putStrLn "A to-do has left our world"

printToDosWord :: String -> [ToDo] -> Int -> IO ()
printToDosWord string [] _ = return ()
printToDosWord string ((ToDoDate name Nothing):xs) n = do
  let number = (show n) ++ ") "
  if string `isInfixOf` name
    then putStrLn (number ++ name)
    else return ()
  printToDosWord string xs (n + 1)
printToDosWord string ((ToDoDate name (Just date)):xs) n = do
  let number = (show n) ++ ") "
  if string `isInfixOf` name
    then putStrLn (number ++ name ++ " " ++ date)
    else return () 
  printToDosWord string xs (n + 1)

---------------------------------------

rewrite :: [ToDo] -> IO ()
rewrite todos = do
  home <- getHomeDirectory
  rewrite' todos home 1

rewrite' :: [ToDo] -> String -> Int -> IO ()
rewrite' [] home n = appendFile (home ++ "/.todo") ""
rewrite' ((ToDoDate name (Just date)):xs) home n = do
  appendFile (home ++ "/.todo") (name ++ "%%" ++ date ++"%%\n")
  rewrite' xs home (n + 1)
rewrite' ((ToDoDate name Nothing):xs) home n = do
  appendFile (home ++ "/.todo") (name ++ "%%\n")
  rewrite' xs home (n + 1)

printToDos :: [ToDo] -> IO ()
printToDos [] = return ()
printToDos todos = do
  printToDos' todos 0

printToDos' :: [ToDo] -> Int -> IO ()
printToDos' [] _ = return ()
printToDos' ((ToDoDate name Nothing):xs) n = do
  let number = (show n) ++ ") "
  putStrLn (number ++ name)
  printToDos' xs (n + 1)
printToDos' ((ToDoDate name (Just date)):xs) n = do
  let number = (show n) ++ ") "
  putStrLn (number ++ name ++ " " ++ date)
  printToDos' xs (n + 1)

readAllToDos :: [String] -> [ToDo]
readAllToDos [] = []
readAllToDos (x:xs) = (readSingleToDo x) : readAllToDos xs

readSingleToDo :: String -> ToDo
readSingleToDo line = do
  let (name:date:empty) = splitOn "%%" line
  if date == "" then
    ToDoDate name Nothing
    else ToDoDate name (Just date)

writeSingleToDo :: ToDo -> IO()
writeSingleToDo (ToDoDate name Nothing) = do
  home <- getHomeDirectory
  let line = name ++ "%%" ++ "\n"
  appendFile (home ++ "/.todo") line
writeSingleToDo (ToDoDate name date) = do
  home <- getHomeDirectory
  let line = name ++ "%%" ++ (fromJust date)++ "%%" ++ "\n"
  appendFile (home ++ "/.todo") line
