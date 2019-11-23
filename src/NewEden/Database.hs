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
{-# LANGUAGE MultiParamTypeClasses #-}
module NewEden.Database
    ( generateNewEden
    ) where

import NewEden.Functions
import NewEden.Types

import Control.Monad (mapM)
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import qualified Data.Int
import Data.Maybe (fromJust)
import qualified Database.SQLite.Simple as SQL
import Text.Heredoc (str)
import Data.String (fromString)
import System.IO.Unsafe (unsafeInterleaveIO)

data DbSolarsystem = DbSolarsystem {
    dbSystemId :: Data.Int.Int32,
    dbSystemName :: String,
    dbSystemCoordX :: Double,
    dbSystemCoordY :: Double,
    dbSystemCoordZ :: Double,
    dbSystemSecurity :: Double,
    dbSystemRegion :: Data.Int.Int32,
    dbSystemRegionName :: String
} deriving (Show)

data DbCelestial = DbCelestial {
    dbCelestialItemID :: Id,
    dbCelestialTypeID :: Id,
    dbCelestialTypeName :: String,
    dbCelestialName :: String,
    dbCelestialCoordX :: Lightyear,
    dbCelestialCoordY :: Lightyear,
    dbCelestialCoordZ :: Lightyear
} deriving (Show)

data DbConnection = DbConnection Id Id

class FromDbType t1 t2 where
    fromDbType :: t1 -> t2

instance FromDbType DbSolarsystem Solarsystem where
    fromDbType db = Solarsystem {
        systemId = dbSystemId db,
        systemName = dbSystemName db,
        systemCoord = Coordinate {
            xCoord = dbSystemCoordX db,
            yCoord = dbSystemCoordY db,
            zCoord = dbSystemCoordZ db
        },
        systemSecurity = dbSystemSecurity db,
        systemRegion = Region {
            regionId = dbSystemRegion db,
            regionName = dbSystemRegionName db
        },
        systemCelestials = []
    }

instance FromDbType DbCelestial Celestial where
    fromDbType db = Celestial {
        celestialItemID = dbCelestialItemID db,
        celestialTypeID = dbCelestialTypeID db,
        celestialTypeName = dbCelestialTypeName db,
        celestialName = dbCelestialName db,
        celestialCoord = Coordinate {
            xCoord = dbCelestialCoordX db,
            yCoord = dbCelestialCoordY db,
            zCoord = dbCelestialCoordZ db
        }
    }

instance SQL.FromRow DbSolarsystem where
    fromRow = DbSolarsystem
                    <$> SQL.field
                    <*> SQL.field
                    <*> SQL.field
                    <*> SQL.field
                    <*> SQL.field
                    <*> SQL.field
                    <*> SQL.field
                    <*> SQL.field


instance SQL.FromRow DbConnection where
    fromRow = DbConnection
                    <$> SQL.field
                    <*> SQL.field

instance SQL.FromRow DbCelestial where
    fromRow = DbCelestial
                    <$> SQL.field
                    <*> SQL.field
                    <*> SQL.field
                    <*> SQL.field
                    <*> SQL.field
                    <*> SQL.field
                    <*> SQL.field

--- | Generates a universe from a local static sqlite database.
--
-- > u <- generateNewEden "/path/to/db.sqlite"
generateNewEden :: String -> IO Universe
generateNewEden dbPath =
    let
        stmSolarSystems :: SQL.Query
        stmSolarSystems = fromString
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
        stmConnections :: SQL.Query
        stmConnections = fromString
            [str|SELECT
                |    fromSolarSystemID,
                |    toSolarSystemID
                |FROM
                |    mapSolarSystemJumps
                |]
    in do
    -- Generate a map of solar systems and then use it to lookup systems
    -- for edges
        hnd <- SQL.open dbPath
        systems_ <- SQL.query_  hnd stmSolarSystems :: IO [DbSolarsystem]
        connections_ <- SQL.query_  hnd stmConnections :: IO [DbConnection]
        SQL.close hnd

        let systems = map fromDbType systems_
        let systemsMap = M.fromList $ map (\k -> (systemId k, k)) systems
        let connections = toConnections systemsMap connections_
        return $ universe systems connections
    where
        toConnections :: LookupMap
                      -> [DbConnection]
                      -> [Connection]
        toConnections systems rows =
            map conv rows
            where
                conv (DbConnection fromId toId) =
                    let
                        a = systems M.! fromId
                        b = systems M.! toId
                    in
                        Connection a b

-- | Generate a list of Celestials from the database
-- genCelestials :: String -> Id -> IO [Celestial]
-- genCelestials dbPath systemId =
--     let
--         stmCelestials =
--             [str|SELECT
--                 |    dn.itemID,
--                 |    it.typeID,
--                 |    it.typeName,
--                 |    n.itemName,
--                 |    dn.x, dn.y, dn.z
--                 |FROM
--                 |    invTypes AS it,
--                 |    mapDenormalize AS dn,
--                 |    invNames AS n
--                 |WHERE
--                 |    it.typeID = dn.typeID
--                 |AND n.itemID = dn.itemID
--                 |AND dn.solarSystemID = ?
--                 |]
--     in do
--     hnd <- SQL.open dbPath
--     resCelestials <- SQL.query_ hnd stmCelestials (SQL.Only systemId) :: IO [DbCelestial]
--     SQL.close hnd
-- 
--     return $ map toCelestial resCelestials
