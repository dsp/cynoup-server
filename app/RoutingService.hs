{-# LANGUAGE OverloadedStrings #-} 
module Main where

import Control.Concurrent.MVar
import Control.Exception (throw)
import Control.Monad
import Data.Maybe
import Network (PortNumber)
import System.IO

import qualified Data.Text.Format as F
import qualified Data.Vector as V

import qualified NewEdenRouting
import qualified NewEden_Types as T
import qualified Thrift.Server as TS

import qualified NewEdenRouting_Iface as S
import NewEden
import RunServer

data ServiceHandler = ServiceHandler {
    neweden :: MVar Universe
}

newHandler :: Universe -> IO ServiceHandler
newHandler u = do
    mu <- newMVar u
    return $ ServiceHandler mu

conv = fromIntegral . systemId

instance S.NewEdenRouting_Iface ServiceHandler where
    route self from to c opts = do
        universe <- readMVar (neweden self)

        -- Construct our temporary universe
        let connections = fromThriftCons c universe
        let reqUniverse = insertConnections universe connections

        let fromS = lookupById (fromIntegral from) reqUniverse
        let toS   = lookupById (fromIntegral to) reqUniverse

        -- Our assertion. All systems must be in the universe
        when (isNothing (toS *> fromS)) $
            throw $ T.LogicalError 1 "Systems not in universe"

        -- Run our dijkstra
        let mpath = route reqUniverse RouteShortest (fromJust fromS, fromJust toS)

        -- Check if we have a result and return the appropriate value
        -- from the service.
        case mpath of
            Just path ->
                return $ V.fromList (map conv path)
            Nothing ->
                return $ V.empty

        where
            fromThriftCons c u = V.toList $ (V.map (convertConnection u) c)
            convertConnection u (T.Connection a b _) =
                let
                    sysA = lookupById (fromIntegral a) u
                    sysB = lookupById (fromIntegral b) u
                in
                Connection (fromJust sysA) (fromJust sysB)

    jumps self from to reach opts = do
        universe <- readMVar (neweden self)

        let fromS = lookupById (fromIntegral from) universe
        let toS   = lookupById (fromIntegral to) universe

        -- Our assertion. All systems must be in the universe
        when (isNothing (toS *> fromS)) $
            throw $ T.LogicalError 1 "Systems not in universe"

        let mpath = jumps universe reach RouteShortest (fromJust fromS, fromJust toS)

        -- Check if we have a result and return the appropriate value
        -- from the service.
        case mpath of
            Just path ->
                return $ V.fromList (map conv path)
            Nothing ->
                return $ V.empty

prgVersion :: String
prgVersion = "0.1.0"

run :: String -> PortNumber -> IO ()
run dbPath port = do
    graphNewEden <- generateNewEden dbPath
    handler <- newHandler $! graphNewEden
    F.print "Cynoup routing server v{}\n" (F.Only prgVersion)
    F.print "Starting the server on port {}...\n" (F.Only (show port))
    hFlush stdout *> hFlush stderr

    TS.runBasicServer handler NewEdenRouting.process port

main :: IO ()
main = runServerFromComandline run
