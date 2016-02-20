{-|
Module      : NewEden.Types
Description : Types used inside the library
Copyright   : (c) Danilaw, 2016
License     : GPLv3
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE DeriveGeneric #-}

module NewEden.Types
    ( AdjacentList
    , Celestial(..)
    , Connection(..)
    , Coordinate(..)
    , DistanceFn
    , Id
    , EstimationFn
    , Lightyear
    , LookupMap
    , Meter
    , Region(..)
    , Route
    , Solarsystem(..)
    , Universe(..)
    ) where

import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import qualified Data.Int
import Data.Hashable (Hashable)

type Lightyear = Double
type Meter = Double
type Id = Data.Int.Int32

data Coordinate = Coordinate {
    xCoord :: Lightyear,
    yCoord :: Lightyear,
    zCoord :: Lightyear
} deriving (Show, Eq, Generic)
instance Hashable Coordinate

data Region = Region {
    regionId :: Id,
    regionName :: String
} deriving (Show, Eq, Generic)
instance Hashable Region

data Solarsystem = Solarsystem {
    systemId :: Id,
    systemName :: String,
    systemCoord :: Coordinate,
    systemSecurity :: Double,
    systemRegion :: Region,
    systemCelestials :: [Celestial]
} deriving (Show, Generic)
instance Hashable Solarsystem

data Celestial = Celestial {
    celestialItemID :: Id,
    celestialTypeID :: Id,
    celestialTypeName :: String,
    celestialName :: String,
    celestialCoord :: Coordinate
} deriving (Show, Generic)
instance Hashable Celestial

instance Eq Solarsystem where
    (==) s1 s2 = (systemId s1) == (systemId s2)
instance Ord Solarsystem where
    (<=) s1 s2 = (systemId s1) <= (systemId s2)

type Route = [Solarsystem]
type AdjacentList = M.HashMap Solarsystem [Solarsystem]
type DistanceFn = (Solarsystem -> Solarsystem -> Double)
type LookupMap = M.HashMap Id Solarsystem
type EstimationFn = (Solarsystem -> Double)

data Universe = Universe {
    solarSystems :: S.Set Solarsystem,
    adjacentList :: AdjacentList,
    lookupMap :: LookupMap
} deriving (Show, Eq)

data Connection = Connection Solarsystem Solarsystem
    deriving(Show, Ord, Eq)

-- Handler for a universes. At the service boarder we move around data points
data UniverseHandle = UniverseHandle String
    deriving(Ord, Eq, Show)
