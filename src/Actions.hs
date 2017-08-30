module Actions( addToDo
                   , listToDos
                   --, doneToDo
                   , help
                   , wrong
                   , killToDo
                   ) where

import Data.Char (isNumber, digitToInt)
import Control.DeepSeq (force)
import System.Directory (getHomeDirectory)
import Control.Exception.Base (evaluate)

import Data.List (isInfixOf, reverse, (\\), delete)

import Data.List.Split (splitOn)

import Data.Maybe

import Types


addToDo :: [String] -> IO ()
addToDo (name:date:empty) = do
  home <- getHomeDirectory
  contents <- readFile (home ++ "/.todo")
  evaluate (force contents)
  writeSingleToDo (ToDoDate name (Just date))
  putStrLn "To-do added"
addToDo (name:empty) = do
  home <- getHomeDirectory
  contents <- readFile (home ++ "/.todo")
  evaluate (force contents)
  writeSingleToDo (ToDoDate name Nothing)
  putStrLn "A to-do joined our world"
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
  putStrLn "ToDo"
  putStrLn "Possible commands: add, list, remove, help"
  putStrLn "Usage: todo <command> [<argument> [<argument> ...]]"
  putStrLn "add <todo>: insert a to-do in the list"
  putStrLn "list: show the list"
  putStrLn "help: show this message"


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
     aaa string todos 0
     else do
      let todos = readAllToDos (lines contents)
      let newTodos = delete (target !! 0) todos
      writeFile (home ++ "/.todo") ""
      rewrite newTodos
      putStrLn "A to-do left our world"

-- temp
aaa :: String -> [ToDo] -> Int -> IO ()
aaa string [] _ = return ()
aaa string ((ToDoDate name Nothing):xs) n = do
  let number = (show n) ++ ") "
  if string `isInfixOf` name
    then putStrLn (number ++ name)
    else return ()
  aaa string xs (n + 1)
aaa string ((ToDoDate name (Just date)):xs) n = do
  let number = (show n) ++ ") "
  if string `isInfixOf` name
    then putStrLn (number ++ name ++ " " ++ date)
    else return () 
  aaa string xs (n + 1)

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


-- Takes a todo or its number
{-
doneToDo :: String -> IO ()
doneToDo input = do
  home <- getHomeDirectory

  case input of "all" -> do writeFile (home ++ "/.todo") ""
                            putStrLn "All to-dos completed"
                otherwise -> do contents <- readFile (home ++ "/.todo")
                                evaluate (force contents)
                                
                                let todos = lines contents

                                -- Ready for some bad code? I am not
                                let res = deleteTodoByNumber input todos
                                
                                if res == todos
                                  then do
                                  let res = deleteTodoByName input todos

                                  if res == todos
                                    then do

                                    if check input res > 1
                                      then do putStrLn "Possible to-dos:\n"
                                              listToDosWithWord input res
                                      else do
                                      let out = deleteTodoByOccurrence input res

                                      if out == res
                                        then putStrLn "The to-do doesn't exist"
                                        else do writeFile (home ++ "/.todo") ""
                                                let reversed = reverse out
                                                rewrite' reversed home
                                                putStrLn ("To-do " ++ head(res\\ reversed) ++ " completed")
                                            
                                    else do
                                    let reversed = reverse res
                                    writeFile (home ++ "/.todo") ""
                                    rewrite' reversed home
                                    putStrLn ("To-do " ++ head(todos \\ reversed) ++ " completed")
                                  
                                  else do let reversed = reverse res
                                          writeFile (home ++ "/.todo") ""
                                          rewrite' reversed home
                                          putStrLn ("To-do " ++ head(todos\\ res) ++ " completed")

-}

-- Substring until first character occurrence
substring :: String -> Char -> String
substring [] _ = []
substring (x:xs) character = if x /= character
                             then [x] ++ substring xs character
                             else []
                                  
deleteTodoByNumber :: String -> [String] -> [String]
deleteTodoByNumber number [] = []
deleteTodoByNumber number (x:xs) = if number == substring x ')'
                                   then deleteTodoByNumber number xs
                                   else [x] ++ deleteTodoByNumber number xs

deleteTodoByName :: String -> [String] -> [String]
deleteTodoByName name [] = []
deleteTodoByName name (x:xs) = if name == drop (length (substring x ')') + 2) x
                               then deleteTodoByName name xs
                               else [x] ++ deleteTodoByName name xs

deleteTodoByOccurrence :: String -> [String] -> [String]
deleteTodoByOccurrence word [] = []
deleteTodoByOccurrence word (x:xs) = if isInfixOf word x
                                     then deleteTodoByOccurrence word xs
                                     else [x] ++ deleteTodoByOccurrence word xs

digitCount :: Integer -> Int
digitCount = go 1 . abs
    where
        go ds n = if n >= 10 then go (ds + 1) (n `div` 10) else ds

check :: String -> [String] -> Int
check word [] = 0
check word (x:xs) = if isInfixOf word x
                    then 1 + check word xs
                    else 0 + check word xs

listToDosWithWord :: String -> [String] -> IO ()
listToDosWithWord word [] = return ()
listToDosWithWord word (x:xs) = if isInfixOf word x
                                then do putStrLn x
                                        listToDosWithWord word xs
                                else listToDosWithWord word xs


