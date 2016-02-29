module Main where
import qualified NewEden.Database as D
import qualified NewEden.Routing as R
import qualified NewEden.Functions as F
import qualified NewEden.Types as T
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Control.Monad (when)

import System.Directory
import System.Environment (lookupEnv)
import System.Exit(exitFailure, exitSuccess)
import Test.HUnit

lookupSystems u xs = 
    let
        l = S.toList (T.solarSystems u)
        m = M.fromList $ map (\a -> (T.systemName a, a)) l
    in
        catMaybes $ map (\b -> M.lookup b m) xs

toNames u xs = 
    maybe [] convert xs
    where convert xs = map T.systemName xs

tests graph = test 
    [
        "Simple reachable " ~: do
            let fasp = fromJust $ F.lookupById 30000044 graph
            let res = Just $ R.reachableSystems 5.0 graph (fasp)
            let names = toNames graph res
            assertEqual ("Range 5 LY from fasp" ++ (show names)) 51 (length names)
    ,
        "Simple dijkstra" ~: do
            let fasp = fromJust $ F.lookupById 30000044 graph
            let fera = fromJust $ F.lookupById 30000050 graph
            let res = R.route graph T.RouteShortest (fasp, fera)
            let names = toNames graph res
            assertEqual ("Lenght Fasp -> Fera is not correct" ++ (show names)) 4 (length names)
    ,
        "Simple dijkstra 2" ~: do
            let fasp = fromJust $ F.lookupById 30000044 graph
            let jita = fromJust $ F.lookupById 30000142 graph
            let res = R.route graph T.RouteShortest (fasp, jita)
            let names = toNames graph res
            assertEqual "Lenght Fasp -> Jita is not correct" 28 (length names)
    ,
        "Simple dijkstra 3" ~: do
            let fasp = fromJust $ F.lookupById 30000044 graph
            let res = R.route graph T.RouteShortest (fasp, fasp)
            let names = toNames graph res
            assertEqual "Lenght Fasp -> Fasp is not correct" 1 (length names)
    ,
        "No route" ~: do
            let fasp = fromJust $ F.lookupById 30000044 graph
            let polaris = fromJust $ F.lookupById 30000380 graph
            let res = R.route graph T.RouteShortest (fasp, polaris)
            assertBool "Fasp -> Polaris must be not reachable" (isNothing res)
    ,
        "Jump Nyx Fasp to HB-5L3" ~: do
            let fasp = fromJust $ F.lookupById 30000044 graph
            let hb5l3 = fromJust $ F.lookupById 30004228 graph
            let res = R.jumps graph 5.0 T.RouteShortest (fasp, hb5l3)
            let names = toNames graph res
            assertEqual ("Jumping from Fasp to HB-5L3" ++ (show names)) 17 (length names)
    ]

main = do
    env <- lookupEnv "CYNOUP_TESTDB"
    let dbPath = maybe "sqlite-latest.sqlite" id env

    exists <- doesFileExist dbPath
    when (not exists)
        exitFailure

    graph <- D.generateNewEden dbPath
    (Counts cases tried errors failures) <- runTestTT (tests graph)
    when ((errors + failures) > 0)
        exitFailure

    exitSuccess
