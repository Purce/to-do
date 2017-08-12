module ToDo.Options where

import System.Console.GetOpt
import System.Exit (exitWith, ExitCode(ExitSuccess))
import System.IO (hPutStrLn, stderr)
import ToDo.Actions

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
                listToDos
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
                let usage = "Usage: to-do -a \"to-do\""
                hPutStrLn stderr (usageInfo usage options)
                exitWith ExitSuccess))
        "Show this help text"
    
    ]
