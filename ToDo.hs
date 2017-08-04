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
        "To-do done"
 
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                let usage = "Usage: to-do -a \"to-do\""
                hPutStrLn stderr (usageInfo usage options)
                exitWith ExitSuccess))
        "Show help"
    
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
  appendFile (home ++ "/.todo") (todo ++ "\n")

doneToDo todo = do
  home <- getHomeDirectory
  contents <- readFile (home ++ "/.todo")
  evaluate (force contents)
  let todos = lines contents
  let result = delete todo todos
  -- clear file
  writeFile (home ++ "/.todo") ""
  rewrite result home
  putStrLn ("DONE " ++ todo)

-- it works, but it's ugly
rewrite [] home = return ()
rewrite (x:xs) home = do appendFile (home ++ "/.todo") (x ++ "\n")
                         rewrite xs home
                 
main :: IO ()
main = do
  args <- getArgs
  -- Parse options, getting a list of option actions
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
  -- Here we thread startOptions through all supplied option actions
  opts <- foldl (>>=) (return startOptions) actions
  putStrLn "Not a valid command.\n-h is all you need"

