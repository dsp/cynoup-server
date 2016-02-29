module NewEden.Routing.Preferences
    ( fromRoutePreference
    , preferShorter
    , preferSafer
    , preferHighsec
    , preferLessSafe
    ) where

import NewEden.Types
    

preferShorter :: Solarsystem -> Solarsystem -> Double
preferShorter _ _ = 1.0


preferSafer :: Solarsystem -> Solarsystem -> Double
preferSafer =
    prefer sec
    where
        sec :: Double -> Double
        sec a
            | a <= 0.0 = 1000.0
            | a <= 0.5 = 100.0
            | otherwise = 1.0


preferLessSafe :: Solarsystem -> Solarsystem -> Double
preferLessSafe =
    prefer sec
    where
        sec :: Double -> Double
        sec a
            | a <= 0.0 = 1.0
            | a <= 0.5 = 100.0
            | otherwise = 1000.0


preferHighsec :: Solarsystem -> Solarsystem -> Double
preferHighsec =
    prefer sec
    where
        sec :: Double -> Double
        sec a
            | a <= 0.0 = 1000.0
            | a <= 0.5 = 1000.0
            | otherwise = 1.0

-- Internal:
prefer :: (Double -> Double) -> Solarsystem -> Solarsystem -> Double
prefer fn a b = fn $ min (systemSecurity a) (systemSecurity b)

fromRoutePreference :: RoutePreference -> DistanceFn
fromRoutePreference pref =
    case pref of
        RouteShortest -> preferShorter
        RouteSafer -> preferHighsec
        RouteMoreDangerous -> preferLessSafe
