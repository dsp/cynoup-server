{-|
Module      : NewEden.Functions
Description : Helper functions
Copyright   : (c) Danilaw, 2016
License     : GPLv3
Stability   : experimental
Portability : POSIX

Various helper functions to construct universe, convert, etc.
-}
module NewEden.Functions
    ( -- module exports
      combine
    , distance
    , fromConnectionList
    , fromMeters
    , insertConnections
    , lookupById
    , member
    , notMember
    , universe
    , universeMaybe
    ) where

import NewEden.Types

import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Data.List (nub)


-- | Converts meters to lightyears.
-- The database dumps from CCP store all coordinates in meters, but we want
-- to internally deal with lightyears.
fromMeters :: Meter -> Lightyear
fromMeters = (flip (/)) 9.4605284e15

toMeters :: Lightyear -> Meter
toMeters = (*) 9.4605284e15

-- | Checks if the given solarsystem is a member of the universe.
member :: Solarsystem -> Universe -> Bool
member s u = S.member s (solarSystems u)

-- | Opposite of 'member'
notMember :: Solarsystem -> Universe -> Bool
notMember s u = S.notMember s (solarSystems u)

-- | Calculates the distance between two coordinates....
distance :: Coordinate -> Coordinate -> Lightyear
distance to from = norm (diff to from)

-- | Constructs an adjacent list from a list of connections.
-- Interally used.
fromConnectionList :: [Connection] -> AdjacentList
fromConnectionList connections =
    M.fromListWith (++) ((map conv connections) ++ (map conv2 connections))
    where
        -- Ensure birectionality
        conv (Connection a b) = (a,[b])
        conv2 (Connection a b) = (b,[a])


-- | Create a universe from a solar system map and an adjacent list.
--   Returns a new universe if all connections are valid within the universe
--   (All connected solar systems exist). Otherwise retursn 'Nothing'
universeMaybe :: [Solarsystem]
              -> [Connection]
              -> Maybe Universe
universeMaybe s c =
    let
        systems = S.fromList s
        connections = fromConnectionList c
        lookupMap = M.fromList $ map conv (S.elems systems)
        cons = concatMap flatten (M.toList connections)
        emptyConnections = M.fromList $ map (\k -> (k,[])) (S.elems systems)
    in
    if all (checkConnection lookupMap) cons then
        Just $
            Universe {
                solarSystems = systems,
                adjacentList = M.union connections emptyConnections,
                lookupMap = lookupMap
            }
    else
        Nothing
    where
        conv k = (systemId k, k)
        flatten (a,xs) = map (\x -> (a,x)) xs
        checkConnection lm (s1,s2) =
            let
                (a, b) = (systemId s1, systemId s2)
            in
                M.member a lm && M.member b lm


-- | Creates a universe from a list of connections
universe :: [Solarsystem]
         -> [Connection]
         -> Universe
universe s c = fromJust $ universeMaybe s c

lookupById :: Id -> Universe -> Maybe Solarsystem
lookupById i u = M.lookup i (lookupMap u)


-- | The `combineUniverses` functions combines two universes by union
--   the solarsystems map and combine adjacent lists. Duplicated entries
--   are removed.
combine :: Universe -> Universe -> Universe
combine u1 u2 =
    let
        (s1, al1, lm1) = (solarSystems u1, adjacentList u1, lookupMap u1)
        (s2, al2, lm2) = (solarSystems u2, adjacentList u2, lookupMap u2)
    in
    Universe {
        solarSystems = S.union s1 s2,
        adjacentList = M.unionWith combine al1 al2,
        lookupMap = M.union lm1 lm2
    }
    where
        combine a b = nub $ a ++ b

insertConnections :: Universe -> [Connection] -> Universe
insertConnections u c =
    let
        connections = fromConnectionList c
    in
    Universe {
        solarSystems = solarSystems u,
        adjacentList = M.unionWith combine (adjacentList u) connections,
        lookupMap = lookupMap u
    }
    where
        combine a b = nub $ a ++ b
