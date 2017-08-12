module ToDo.Actions( addToDo
                   , listToDos
                   , doneToDo
                   ) where

import Data.Char (isNumber)
import Control.DeepSeq (force)
import System.Directory (getHomeDirectory)
import Control.Exception.Base (evaluate)

listToDos :: IO ()
listToDos = do
  putStrLn "To-dos"
  home <- getHomeDirectory
  contents <- readFile (home ++ "/.todo")
  putStrLn contents


addToDo :: String -> IO ()
addToDo todo = do
  home <- getHomeDirectory
  contents <- readFile (home ++ "/.todo")
  evaluate (force contents)
  let number = length (lines contents)
  appendFile (home ++ "/.todo") (show (number + 1) ++ ") " ++ todo ++ "\n")
  putStrLn ("To-do added")
  
-- takes a todo or its number
doneToDo :: String -> IO ()
doneToDo input = do
  home <- getHomeDirectory

  case input of "all" -> do writeFile (home ++ "/.todo") ""
                            putStrLn "All to-dos completed"
                otherwise -> do contents <- readFile (home ++ "/.todo")
                                evaluate (force contents)
                                let todos = lines contents

                                -- bad, but it works
                                let res = deleteTodoByNumber input todos
                                if res == todos
                                  then putStrLn "The to-do doesn't exist"
                                  else putStrLn "To-do completed"
                                
                                writeFile (home ++ "/.todo") ""
                                rewrite res home 1
                                
                                let res = deleteTodoByName input todos
                                if res == todos
                                  then putStrLn "The to-do doesn't exist"
                                  else putStrLn "To-do completed"
                                  
                                writeFile (home ++ "/.todo") ""
                                rewrite res home 1


-- Substring until first character occurrence
substring :: String -> Char -> String
substring [] _ = []
substring (x:xs) character = if x /= character
                             then [x] ++ substring xs character
                             else []
                                  
-- Is it possible to merge these two functions?
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

-- https://stackoverflow.com/questions/25005778/most-efficient-way-to-get-digit-count-of-arbitrarily-big-number
digitCount :: Integer -> Int
digitCount = go 1 . abs
    where
        go ds n = if n >= 10 then go (ds + 1) (n `div` 10) else ds
        
-- it works, but it's ugly
rewrite :: [String] -> String -> Integer -> IO ()
rewrite [] home n = appendFile (home ++ "/.todo") ""
rewrite (x:xs) home n = do appendFile (home ++ "/.todo") (show n ++ ") " ++ drop (2 + digitCount n) x ++"\n")
                           rewrite xs home (n + 1)
