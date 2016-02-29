{-|
Module      : NewEden.Database
Description : Simple interface to Eve onlines static dump
Copyright   : (c) Danilaw, 2016
License     : GPLv3
Stability   : experimental
Portability : POSIX

A simple interface to read the map of New Eden from an SQLite static dump.
-}
{-# LANGUAGE QuasiQuotes #-}
module NewEden.Database
    ( generateNewEden
    ) where

import NewEden.Functions
import NewEden.Types

import Control.Monad (mapM)
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Database.HDBC (fromSql, quickQuery', quickQuery, disconnect, SqlValue, safeFromSql)
import Database.HDBC.Sqlite3 (connectSqlite3)
import Text.Heredoc (str)
import System.IO.Unsafe (unsafeInterleaveIO)


-- | Generates a universe from a local static sqlite database.
--
-- > u <- generateNewEden "/path/to/db.sqlite"
generateNewEden :: String -> IO Universe
generateNewEden dbPath =
    let
        stmSolarSystems =
            [str|SELECT
                |    src.solarSystemID,
                |    src.solarSystemName,
                |    src.x,
                |    src.y,
                |    src.z,
                |    src.security,
                |    src.regionID,
                |    r.regionName
                |FROM
                |    mapSolarSystems AS src,
                |    mapRegions AS r
                |WHERE
                |    src.regionID = r.regionID
                |]
        stmConnections =
            [str|SELECT
                |    fromSolarSystemID,
                |    toSolarSystemID
                |FROM
                |    mapSolarSystemJumps
                |]
    in do
    -- Generate a map of solar systems and then use it to lookup systems
    -- for edges
        hnd <- connectSqlite3 dbPath
        resSolarSystems <- quickQuery' hnd stmSolarSystems []
        resConnections <- quickQuery' hnd stmConnections []
        disconnect hnd

        systems <- toSolarSystems resSolarSystems
        let systemsMap = M.fromList $ map (\k -> (systemId k, k)) systems
        let connections = toConnections systemsMap resConnections

        return $ universe systems connections
    where

        -- get a map of solar systems the solarsystem id as the key
        -- TODO: make types clearer
        toSolarSystems :: [[SqlValue]] -> IO [Solarsystem]
        toSolarSystems rows =
            mapM toSolarSystem rows
            where
                -- Convert a row to a solar system
                toSolarSystem :: [SqlValue] -> IO Solarsystem
                toSolarSystem [id, name, x, y, z, sec, regid, regname] = do
                    -- TODO: not really sure how to do the lazy loading of celestials yet
                    --celestials <- unsafeInterleaveIO $ genCelestials dbPath id
                    return $ Solarsystem {
                        systemId = (fromSql id) :: Id,
                        systemName = (fromSql name) :: String,
                        systemCoord = Coordinate {
                            xCoord = fromMeters (fromSql x),
                            yCoord = fromMeters (fromSql y),
                            zCoord = fromMeters (fromSql z)
                        },
                        systemSecurity = (fromSql sec) :: Double,
                        systemRegion = Region {
                            regionId = (fromSql regid) :: Id,
                            regionName = (fromSql regname) :: String
                        },
                        systemCelestials = [] -- celestials
                    }

        toConnections :: LookupMap
                      -> [[SqlValue]]
                      -> [Connection]
        toConnections systems rows =
            map conv rows
            where
                conv [fromId, toId] =
                    let
                        a = systems M.! (fromSql fromId :: Id)
                        b = systems M.! (fromSql toId :: Id)
                    in
                        Connection a b

-- | Generate a list of Celestials from the database
genCelestials :: String -> SqlValue -> IO [Celestial]
genCelestials dbPath systemId =
    let
        stmCelestials =
            [str|SELECT
                |    dn.itemID,
                |    it.typeID,
                |    it.typeName,
                |    n.itemName,
                |    dn.x, dn.y, dn.z
                |FROM
                |    invTypes AS it,
                |    mapDenormalize AS dn,
                |    invNames AS n
                |WHERE
                |    it.typeID = dn.typeID
                |AND n.itemID = dn.itemID
                |AND dn.solarSystemID = ?
                |]
    in do

    hnd <- connectSqlite3 dbPath
    resCelestials <- quickQuery' hnd stmCelestials [systemId]
    disconnect hnd

    return $ map toCelestial resCelestials
    where
        toCelestial :: [SqlValue] -> Celestial
        toCelestial [itemId, typeId, typeName, itemName, x, y, z] =
            let
                name = either (const "") id (safeFromSql itemName)
            in
            Celestial {
                celestialItemID = fromSql itemId,
                celestialTypeID = fromSql typeId,
                celestialTypeName = fromSql typeName,
                celestialName  = name,
                celestialCoord = Coordinate {
                    xCoord = fromMeters (fromSql x),
                    yCoord = fromMeters (fromSql y),
                    zCoord = fromMeters (fromSql z)
                }
            }
