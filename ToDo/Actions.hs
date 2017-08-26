module ToDo.Actions( addToDo
                   , listToDos
                   , doneToDo
                   , help
                   , wrong
                   ) where

import Data.Char (isNumber, digitToInt)
import Control.DeepSeq (force)
import System.Directory (getHomeDirectory)
import Control.Exception.Base (evaluate)

import Data.List (isInfixOf, reverse, (\\))

import ToDo.ToDo

addToDo :: String -> IO ()
addToDo name = do
  home <- getHomeDirectory
  contents <- readFile (home ++ "/.todo")
  evaluate (force contents)
  let number = length (lines contents) + 1
  appendFile (home ++ "/.todo") (show (ToDo number name) ++ "\n")
  putStrLn "To-do added"

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
---------------------------------------

stringToInt :: String -> Int
stringToInt string = stringToInt' (reverse string) 10 0

stringToInt' :: String -> Int -> Int -> Int
stringToInt' [] _ _ = 0
stringToInt' (x:xs) base n = base ^ n * (digitToInt x) +
                             (stringToInt' xs base (n + 1))

printToDos :: [ToDo] -> IO ()
printToDos [] = return ()
printToDos (x:xs) = do
  putStrLn (show x)
  printToDos xs

readAllToDos :: [String] -> [ToDo]
readAllToDos [] = []
readAllToDos (x:xs) = (readSingleToDo x) : readAllToDos xs

readSingleToDo :: String -> ToDo
readSingleToDo line = do
  let number = readNumber line ')'
  let name = readName line ')'
  let num = stringToInt number
  ToDo num name

readNumber :: String -> Char -> String
readNumber [] _ = []
readNumber (x:xs) character = if x /= character 
                               then x : readNumber xs character
                               else []
                                  
readName :: String -> Char -> String
readName [] _ = []
readName (x:xs) character = if x == character
                             then tail xs
                             else readName xs character



  
-- Takes a todo or its number
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
        
-- it works, but it's ugly
rewrite :: [String] -> String -> Integer -> IO ()
rewrite [] home n = appendFile (home ++ "/.todo") ""
rewrite (x:xs) home n = do appendFile (home ++ "/.todo")
                             (show n ++ ") " ++
                              drop (2 + digitCount n) x ++"\n")
                           rewrite xs home (n + 1)

-- it works, but i must reverse the list before calling it
rewrite' :: [String] -> String -> IO ()
rewrite' [] home = appendFile (home ++ "/.todo") ""
rewrite' (x:xs) home = do rewrite' xs home
                          appendFile (home ++ "/.todo")
                            (show (length xs + 1) ++
                             (drop (length (substring x ')')) x) ++ "\n")

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


