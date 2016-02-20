{-# LANGUAGE OverloadedStrings #-} 

module RunServer 
  (
    runServerFromComandline
  ) where

import Control.Monad
import Data.Text.Format as F
import Network (PortNumber)
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO

import qualified NewEdenRouting

import Thrift
import Thrift.Protocol.Binary
import Thrift.Transport
import Thrift.Server

data Options = Options  { optVerbose  :: Bool
                        , optDatabase :: String
                        , optPort     :: PortNumber
                        }

startOptions :: Options
startOptions = Options  { optVerbose  = False
                        , optDatabase = "neweden-latest.sqlite"
                        , optPort     = 9090
                        }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "f" ["file"]
        (ReqArg
            (\arg opt -> return opt { optDatabase = arg })
            "FILE")
        "Path to static Sqlite database of New Eden"

    , Option "p" ["port"]
        (ReqArg
            (\arg opt -> return opt { optPort = fromInteger (read arg) })
            "PORT")
        "Port to run on (default: 9090)"

    , Option "v" ["verbose"]
        (NoArg
            (\opt -> return opt { optVerbose = True }))
        "Enable verbose messages"

    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"
    ]

--runServer :: IO ()
runServerFromComandline serverProcess = do
    args <- getArgs

    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt RequireOrder options args

    opts <- foldl (>>=) (return startOptions) actions

    let Options { optVerbose = verbose
                , optDatabase = dbPath
                , optPort = port
                } = opts

    -- Validate our options
    exists <- doesFileExist dbPath
    unless exists (do
        hprint stderr "Database path '{}' does not exist\n" (Only dbPath)
        exitWith (ExitFailure 1))

    -- Generate new eden and start the server
    serverProcess dbPath port

    putStrLn "done."
