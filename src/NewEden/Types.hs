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
    , diff
    , distance
    , DistanceFn
    , DistanceList
    , DistancePair(..)
    , EstimationFn
    , Id
    , Lightyear
    , LookupMap
    , Meter
    , NeighbourFn
    , norm
    , NormedVectorSpace
    , Region(..)
    , Route
    , RoutePreference(..)
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

data Region = Region {
    regionId :: Id,
    regionName :: String
} deriving (Show, Eq, Generic)

data Solarsystem = Solarsystem {
    systemId :: Id,
    systemName :: String,
    systemCoord :: Coordinate,
    systemSecurity :: Double,
    systemRegion :: Region,
    systemCelestials :: [Celestial]
} deriving (Show, Generic)

data Celestial = Celestial {
    celestialItemID :: Id,
    celestialTypeID :: Id,
    celestialTypeName :: String,
    celestialName :: String,
    celestialCoord :: Coordinate
} deriving (Show, Generic)

instance Hashable Celestial
instance Hashable Coordinate
instance Hashable Region
instance Hashable Solarsystem

instance Eq Solarsystem where
    (==) s1 s2 = (systemId s1) == (systemId s2)
instance Ord Solarsystem where
    (<=) s1 s2 = (systemId s1) <= (systemId s2)

type Route = [Solarsystem]
type AdjacentList = M.HashMap Solarsystem [Solarsystem]
type DistanceFn = (Solarsystem -> Solarsystem -> Double)
type NeighbourFn = (Universe -> Solarsystem -> [Solarsystem])
type LookupMap = M.HashMap Id Solarsystem
type DistanceList = M.HashMap Solarsystem [DistancePair]
type EstimationFn = (Solarsystem -> Double)

data DistancePair = DistancePair {
        dpDistance :: Lightyear,
        dpSystem :: Solarsystem
    } deriving (Show)
instance Eq DistancePair where
    (==) d1 d2 = dpSystem d1 == dpSystem d2
instance Ord DistancePair where
    (<=) d1 d2 = dpDistance d1 <= dpDistance d2 

data Universe = Universe {
    solarSystems :: S.Set Solarsystem,
    adjacentList :: AdjacentList,
    distanceList :: DistanceList,
    lookupMap :: LookupMap
} deriving (Show, Eq)

data Connection = Connection Solarsystem Solarsystem
    deriving(Show, Ord, Eq)

-- Handler for a universes. At the service boarder we move around data points
data UniverseHandle = UniverseHandle String
    deriving(Ord, Eq, Show)

-- | A partial defintion of a normed vector space which defines a metric
class NormedVectorSpace v where
    norm  :: v -> Double
    diff :: v -> v -> Coordinate
    distance :: v -> v -> Double
    distance a b = norm (diff a b)

instance NormedVectorSpace Coordinate where
    norm (Coordinate x y z) = sqrt(x*x + y*y + z*z)
    diff (Coordinate x0 y0 z0) (Coordinate x1 y1 z1) =
        Coordinate
            (x1-x0)
            (y1-y0)
            (z1-z0)

instance NormedVectorSpace Solarsystem where
    norm a = (norm . systemCoord) a
    diff a b = diff (systemCoord a) (systemCoord b)

data RoutePreference = RouteShortest
                     | RouteSafer
                     | RouteMoreDangerous
