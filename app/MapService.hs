{-# LANGUAGE OverloadedStrings #-} 
module Main where

import Control.Concurrent.MVar
import Control.Exception (throw)
import Control.Monad
import Data.Maybe
import Data.Int
import qualified Data.Text.Lazy as TL
import Network (PortNumber)
import System.IO
import qualified Data.Text.Format as F
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S

import qualified Convert as Conv
import qualified NewEdenMap
import qualified NewEden_Types as T
import qualified NewEdenMapService_Types as TM
import qualified Thrift.Server as TS

import NewEdenMap_Iface
import NewEden
import RunServer

data ServiceHandler = ServiceHandler {
    neweden :: MVar Universe
}

newHandler :: Universe -> IO ServiceHandler
newHandler u = do
    mu <- newMVar u
    return $ ServiceHandler mu

instance NewEdenMap_Iface ServiceHandler where
    systems self = do
        universe <- readMVar (neweden self)
        return $ HM.map Conv.toThriftSolarSystem (lookupMap universe)

    connection self = do
        return (HM.empty :: HM.HashMap Int32 T.Connection)

    closest self systemId coord = do
        universe <- readMVar (neweden self)
        let system = lookupById systemId universe
        when (isNothing system) $
            throw $ T.LogicalError 1 "Systems not in universe"
        let closest = between (fromJust system) (Conv.fromThriftCoordinate coord)
        case closest of
            [] -> return $ V.empty
            ((a,b):_) -> return $
                V.fromList (map Conv.toThriftCelestial [a,b])

    closestCelestial self coord = do
        return $ T.Celestial 0 "sun"

    distance self celestial coord = do
        return 0.0

    toNames self systems = do
        return $ HM.empty
--        V.map systemName $ V.mapMaybe (flip lookupById universe) systems

    toIds self systems = do
        return $ HM.empty
--        V.map systemName $ V.mapMaybe (flip lookupById universe) systems

prgVersion :: String
prgVersion = "0.0.1"

run :: String -> PortNumber -> IO ()
run dbPath port = do
    graphNewEden <- generateNewEden dbPath
    handler <- newHandler $! graphNewEden
    F.print "Cynoup map server v{}\n" (F.Only prgVersion)
    F.print "Starting the server on port {}...\n" (F.Only (show port))
    hFlush stdout *> hFlush stderr

    TS.runBasicServer handler NewEdenMap.process port

main :: IO ()
main = runServerFromComandline run
