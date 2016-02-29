{-|
Module      : NewEden.Routing
Description : Shortest path for New Eden
Copyright   : (c) Danilaw, 2016
License     : GPLv3
Stability   : experimental
Portability : POSIX

A shortest path implementation for solar systems in side Eve Onlines galaxy.
-}
module NewEden.Routing
    ( adjacentSystems
    , astar
    , dijkstra
    , equalDistance
    , jumps
    , reachableSystems
    , route
    ) where

import NewEden.Types
import qualified NewEden.Functions as NE

import Control.Monad
import Control.Monad.State
import Data.Maybe
import qualified Data.Heap as H
import qualified Data.HashMap.Strict as M
import qualified NewEden.Routing.Preferences as Pref

-- TODO: I think the naming is wrong, we want to fix that.
equalDistance :: Solarsystem -> Double
equalDistance _ = 0.0

adjacentSystems u s = (adjacentList u M.! s)

-- TODO: fix this ugly stuff here.
reachableSystems :: Lightyear -> Universe -> Solarsystem -> [Solarsystem]
reachableSystems d u s = 
    let
        pred = distancePred (<= d)
        list = (distanceList u) M.! s
    in
    map dpSystem $
        takeWhile pred list
    where
        distancePred :: (Lightyear -> Bool) -> DistancePair -> Bool
        distancePred pred = dpred
            where
                dpred (DistancePair d1 _) = pred d1


type DijkstraHeap = H.MinPrioHeap Double Solarsystem
type DijkstraState = (DijkstraHeap,
                      M.HashMap Solarsystem Double,
                      M.HashMap Solarsystem Solarsystem)
type ReversePath = M.HashMap Solarsystem Solarsystem

route :: Universe
      -> RoutePreference
      -> (Solarsystem, Solarsystem)
      -> Maybe [Solarsystem]
route u pref (from, to) =
    dijkstra u (Pref.fromRoutePreference pref) adjacentSystems (from, to)

jumps :: Universe
      -> Lightyear
      -> RoutePreference
      -> (Solarsystem, Solarsystem)
      -> Maybe [Solarsystem]
jumps u d pref (from, to) =
    astar u (Pref.fromRoutePreference pref) equalDistance (reachableSystems d) (from, to)

-- | An implementation of Dijkstra's shortes path algorithm. The distance
-- function determines the distance between two solarsystems. The most common
-- application is using the constant 1.0 for system jumps. For capital jumps
-- an apprioriate eucledian distance can be very useful.
dijkstra :: Universe
         -> DistanceFn
         -> NeighbourFn
         -> (Solarsystem, Solarsystem)
         -> Maybe [Solarsystem]
dijkstra u distFn neighbourFn (from, to) =
    astar u distFn equalDistance neighbourFn (from, to)

astar :: Universe
      -> DistanceFn
      -> EstimationFn
      -> NeighbourFn
      -> (Solarsystem, Solarsystem)
      -> Maybe [Solarsystem]
astar universe distFn estFn neighbourFn (from, to)
  -- Invariant: everything needs to exist in theu niverse
  | NE.notMember from universe ||
    NE.notMember to universe = Nothing

  | otherwise =
    let
        state = (H.singleton (0.0, from),
                 M.singleton from 0,
                 M.empty) :: DijkstraState
    in

    suchThat $
        reverse $
            constructPaths to $ evalState astar' state

    where
        constructPaths :: Solarsystem
                       -> M.HashMap Solarsystem Solarsystem
                       -> [Solarsystem]
        constructPaths v paths
            | v == from = [v]
            | M.member v paths = v : constructPaths (paths M.! v) paths
            | otherwise = []

        astar' :: State DijkstraState ReversePath
        astar' = do
            mnext <- nextSolarsystem
            case mnext of
                -- Return path if the heap is empty
                Nothing -> do gets usePath

                -- iterate over all neighbours and relax inside the State
                -- and recurse into dijkstra' again
                Just current ->
                    -- Potential new eden related optimization to always
                    -- minimze for jumps and not for distance. Needs better API
                    ---- | to == current -> do gets usePath
                    do
                        forM_
--                            (adjacentList universe M.! current)
                            (neighbourFn universe current)
                            (relax current)
                        astar'

            where
                usePath (_,_,p) = p

                relax :: Solarsystem
                      -> Solarsystem
                      -> State DijkstraState ()
                relax cur next = do
                    (h, d, p) <- get
                    let alt = (d M.! cur) + (distFn cur next) + (estFn next)
                    let nextDist = M.lookup next d
                    if isNothing nextDist || alt < fromJust nextDist then
                        put (H.insert (alt, next) h, -- add to queue
                            M.insert next alt d, -- insert our new distances
                            M.insert next cur p) -- add a path element
                    else
                        put (h,d,p)

-- | Selects the next solarsystem from the current state and modifies
-- the state.
nextSolarsystem :: State DijkstraState (Maybe Solarsystem)
nextSolarsystem = do
    (h, d, p) <- get
    let (mnext, h') = splitMin h
    put (h', d, p)
    return mnext

-- | Splits the heap from the current state.
splitMin :: DijkstraHeap -> (Maybe Solarsystem, DijkstraHeap)
splitMin h =
    let (a,b) = H.splitAt 1 h in
    case a of
        [] -> (Nothing, b)
        otherwise -> (Just ((snd . head) a), b)

-- | Checks if foldable is null and returns Nothing or Just.
suchThat :: Foldable t => t a -> Maybe (t a)
suchThat a = if (not . null) a then Just a else Nothing

