module ToDo where

import Control.Monad

import Data.List
import Data.Char

import System.Environment
import System.Directory
import System.Console.GetOpt
import System.IO
import System.Exit

import Control.DeepSeq
import Control.Exception

-- https://wiki.haskell.org/High-level_option_handling_with_GetOpt
data Options = Options { optVerbose :: Bool
                       , optInput :: IO String
                       , optOutput :: String -> IO ()
                       }

startOptions :: Options
startOptions = Options { optVerbose = False
                       , optInput = getContents
                       , optOutput = putStr
                       }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "a" ["add"]
        (ReqArg
            (\arg opt -> do addToDo arg
                            exitWith ExitSuccess)
            "TO-DO")
        "To-do to add"
 
    , Option "l" ["list"]
        (NoArg
            (\_ -> do
                printList
                exitWith ExitSuccess
            ))
        "The list of to-dos"
 
    , Option "d" ["done"]
        (ReqArg
            (\arg opt -> do doneToDo arg
                            exitWith ExitSuccess)
            "TO-DO")
        "To-do completed"
 
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                let usage = "Usage: to-do -a \"to-do\""
                hPutStrLn stderr (usageInfo usage options)
                exitWith ExitSuccess))
        "Show this help text"
    
    ]


printList :: IO ()
printList = do
  putStrLn "LIST"
  home <- getHomeDirectory
  contents <- readFile (home ++ "/.todo")
  putStrLn contents


addToDo :: [Char] -> IO ()
addToDo todo = do
  putStrLn ("ADD " ++ todo)
  home <- getHomeDirectory
  contents <- readFile (home ++ "/.todo")
  evaluate (force contents)
  let number = length (lines contents)
  appendFile (home ++ "/.todo") (show (number + 1) ++ ") " ++ todo ++ "\n")

-- takes a todo or its number
doneToDo :: String -> IO ()
doneToDo input = do
  home <- getHomeDirectory
  contents <- readFile (home ++ "/.todo")
  evaluate (force contents)
  let todos = lines contents
  -- input deve essere dato nella forma "'1'". perche?
  -- sistemato, vedi sotto
  let res = deleteTodoByNumber (head input) todos

  if res == todos
    then putStrLn "no"
    else do putStrLn "yes"
            writeFile (home ++ "/.todo") ""
            rewrite res home 1
            putStrLn ("DONE " ++ input)


-- deleteTodo :: String -> [String] -> [String]
-- deleteTodo number todos = do
--   if number == show (head (head todos))
--     then return (delete (head todos) todos)
--     else deleteTodo number (tail todos)

deleteTodoByNumber :: Char -> [String] -> [String]
deleteTodoByNumber number [] = []
deleteTodoByNumber number (x:xs) = if number == head x -- non devo fare show (head x) perche mi da "'2'"
                                   then deleteTodoByNumber number xs
                                   else [x] ++ deleteTodoByNumber number xs

-- deleteTodoByName name [] = return False
-- deleteTodoByName name (x:xs) = if name == (drop 3 x)
--                                then do delete x
--                                        return True
--                                else deleteTodoByName name xs

-- it works, but it's ugly
rewrite [] home n = return ()
rewrite (x:xs) home n = do appendFile (home ++ "/.todo") (show n ++ ") " ++ drop 3 x ++"\n")
                           rewrite xs home (n + 1)
                 
main :: IO ()
main = do
  
  args <- getArgs
  -- Parse options, getting a list of option actions
  let (actions, nonOptions, errors) = getOpt RequireOrder options args

  home <- getHomeDirectory
  check <- doesFileExist (home ++ "/.todo")
  if check
    -- perche devo fare cosi
    -- IO da solo dei problemi
    then return ()
    else writeFile (home ++ "/.todo") ""
    
  -- Here we thread startOptions through all supplied option actions
  opts <- foldl (>>=) (return startOptions) actions
  putStrLn "Not a valid command.\n-h is all you need"

