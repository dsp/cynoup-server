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
import Data.List (nub, sort)


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
                -- TODO: Have to think about this again, it makes the amount of systems
                -- to consider much smaller but assumes eve game limits in the API.
                distanceList = distances (filter jumpableSystems (S.toList systems)),
                lookupMap = lookupMap
            }
    else
        Nothing
    where
        conv k = (systemId k, k)
        flatten (a,xs) = map (\x -> (a,x)) xs
        -- So this is absolutly terrible.
        -- 1. We should check if we have already inserted the reverse distance and reuse it
        -- 2. We put game limitations in here, to reduce the amount of calculations
        -- effectivly ignoring everything further than 10LY as this is the max a ship can do anyway.
        -- 3. It's just horrible code.
        distances systems =
            foldr
                (\s m -> M.insert s (calculateDistances s systems) m)
                M.empty
                systems
            where
                calculateDistances s xs =
                    sort $
                        filter maxJumpDistanceInEve $
                            map (distancePair s) xs

        checkConnection lm (s1,s2) =
            let
                (a, b) = (systemId s1, systemId s2)
            in
                M.member a lm && M.member b lm


maxJumpDistanceInEve = (<= 10.0) . dpDistance
jumpableSystems = (< 0.45) . systemSecurity

distancePair :: Solarsystem -> Solarsystem -> DistancePair
distancePair a b = DistancePair (distance a b) b

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
        (s1, al1, dl1, lm1) = (solarSystems u1, adjacentList u1, distanceList u1, lookupMap u1)
        (s2, al2, dl2, lm2) = (solarSystems u2, adjacentList u2, distanceList u2, lookupMap u2)
    in
    Universe {
        solarSystems = S.union s1 s2,
        adjacentList = M.unionWith combine al1 al2,
        lookupMap = M.union lm1 lm2,
        distanceList = M.unionWith (\a b -> sort (combine a b)) dl1 dl2
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
        distanceList = distanceList u,
        lookupMap = lookupMap u
    }
    where
        combine a b = nub $ a ++ b
