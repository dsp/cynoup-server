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
    ( dijkstra
    , preferHighsec
    , preferSafer
    , preferShorter
    , preferShortestDistanceToDestination
    ) where

import NewEden.Types
import qualified NewEden.Functions as NE

import Control.Monad
import Control.Monad.State
import Data.Maybe
import qualified Data.Heap as H
import qualified Data.HashMap.Strict as M


type DijkstraHeap = H.MinPrioHeap Double Solarsystem
type DijkstraState = (DijkstraHeap,
                      M.HashMap Solarsystem Double,
                      M.HashMap Solarsystem Solarsystem)
type ReversePath = M.HashMap Solarsystem Solarsystem


prefer :: (Double -> Double) -> Solarsystem -> Solarsystem -> Double
prefer fn a b = fn $ min (systemSecurity a) (systemSecurity b)


preferShorter :: Solarsystem -> Solarsystem -> Double
preferShorter _ _ = 1.0

preferShortestDistanceToDestination :: Solarsystem -> Double
preferShortestDistanceToDestination _ = 0.0

preferSafer :: Solarsystem -> Solarsystem -> Double
preferSafer =
    prefer sec
    where
        sec :: Double -> Double
        sec a
            | a <= 0.0 = 1000.0
            | a <= 0.5 = 100.0
            | otherwise = 1.0


preferHighsec :: Solarsystem -> Solarsystem -> Double
preferHighsec =
    prefer sec
    where
        sec :: Double -> Double
        sec a
            | a <= 0.0 = 1000.0
            | a <= 0.5 = 1000.0
            | otherwise = 1.0


-- | An implementation of Dijkstra's shortes path algorithm. The distance
-- function determines the distance between two solarsystems. The most common
-- application is using the constant 1.0 for system jumps. For capital jumps
-- an apprioriate eucledian distance can be very useful.
--
-- Example:
--
-- > u <- generateNewEden "/path/to/db.sqlite"
-- > let lookup = lookupMap u
-- > dijksta u (\_ _ -> 1.0) (systems ! "Faspera", systems Map.! "Jita")
dijkstra :: Universe
         -> DistanceFn
         -> EstimationFn
         -> (Solarsystem, Solarsystem)
         -> Maybe [Solarsystem]
dijkstra universe fn hn (from, to)
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
            constructPaths to $ evalState dijkstra' state

    where
        constructPaths :: Solarsystem
                       -> M.HashMap Solarsystem Solarsystem
                       -> [Solarsystem]
        constructPaths v paths
            | v == from = [v]
            | M.member v paths = v : constructPaths (paths M.! v) paths
            | otherwise = []

        dijkstra' :: State DijkstraState ReversePath
        dijkstra' = do
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
                            (adjacentList universe M.! current)
                            (relax current)
                        dijkstra'

            where
                usePath (_,_,p) = p

                relax :: Solarsystem
                      -> Solarsystem
                      -> State DijkstraState ()
                relax cur next = do
                    (h, d, p) <- get
                    let alt = (d M.! cur) + (fn cur next) + (hn next)
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


