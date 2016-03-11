module NewEden.Routing.Preferences
    ( fromRoutePreference
    , preferShorter
    , preferSafer
    , preferHighsec
    , preferLessSafe
    ) where

import NewEden.Types

import qualified Data.HashMap.Strict as HM

preferShorter :: Universe -> Solarsystem -> Solarsystem -> Double
preferShorter u a b =
    maybe 1.0 id (HM.lookup (a,b) (weights u))


preferSafer :: Universe -> Solarsystem -> Solarsystem -> Double
preferSafer =
    prefer sec
    where
        sec :: Double -> Double
        sec a
            | a <= 0.0 = 1000.0
            | a <= 0.5 = 100.0
            | otherwise = 1.0


preferLessSafe :: Universe -> Solarsystem -> Solarsystem -> Double
preferLessSafe =
    prefer sec
    where
        sec :: Double -> Double
        sec a
            | a <= 0.0 = 1.0
            | a <= 0.5 = 100.0
            | otherwise = 1000.0


preferHighsec :: Universe -> Solarsystem -> Solarsystem -> Double
preferHighsec =
    prefer sec
    where
        sec :: Double -> Double
        sec a
            | a <= 0.0 = 1000.0
            | a <= 0.5 = 1000.0
            | otherwise = 1.0

-- Internal:
prefer :: (Double -> Double) -> Universe -> Solarsystem -> Solarsystem -> Double
prefer fn u a b = 
    multiplier * (preferShorter u a b)
    where
        multiplier = fn $ min (systemSecurity a) (systemSecurity b)

fromRoutePreference :: RoutePreference -> DistanceFn
fromRoutePreference pref =
    case pref of
        RouteShortest -> preferShorter
        RouteSafer -> preferHighsec
        RouteMoreDangerous -> preferLessSafe
