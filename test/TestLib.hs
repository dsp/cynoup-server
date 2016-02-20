{-# LANGUAGE TemplateHaskell #-}
module TestLib
    ( testLibSuite
    ) where

import NewEden
import Control.Monad
import Debug.Trace

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (maybe, isJust, fromJust)
import Data.List (nub)
import Test.QuickCheck
import Test.QuickCheck.All


instance Arbitrary Coordinate where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return $ Coordinate x y z


instance Arbitrary Region where
    arbitrary = do
        id <- arbitrary
        name <- arbitrary
        return $ Region id name


instance Arbitrary Solarsystem where
    arbitrary = do
        id <- arbitrary
        name <- arbitrary
        coord <- arbitrary
        security <- choose (-1.0, 1.0)
        region <- arbitrary
        return $ Solarsystem id name coord security region []


instance Arbitrary Connection where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Connection a b


instance Arbitrary Universe where
    arbitrary = do
        systems <- listOf1 $ arbitrary
        connections <- sublistOf $ [Connection a b | a <- systems,
                                                     b <- systems,
                                                     a /= b]
        let u = universeMaybe systems connections
        if isJust u then
            return $ fromJust u
        else
            arbitrary


prop_distance_pos c1 c2 =
    distance c1 c2 >= 0.0


prop_distance_eucledian c1 c2 =
    let
        zero = Coordinate 0.0 0.0 0.0
    in
    distance zero c2 <= (distance zero c1 + distance zero c2)


prop_fromConnectionList :: [Connection] -> Property
prop_fromConnectionList xs =
    not (null xs) ==>
        let
            Connection a b = head (nub xs)
            adjacentToA = (fromConnectionList xs) M.! a
         in
            (b `elem` adjacentToA)


weightfn _ _ = 1.0
distancefn _ = 0.0
prop_dijkstra2 u a b =
    maybe True ((> 0) . length) (dijkstra u weightfn distancefn (a,b))

prop_dijkstra u =
    length (solarSystems u) >= 3 ==>
        let
            [a, b, c] = take 3 (S.toList (solarSystems u))
            direct = dijkstra u weightfn distancefn (a,c)
            path1 = dijkstra u weightfn distancefn (a,b)
            path2 = dijkstra u weightfn distancefn (b,c)
        in
            (length direct) <= (length path1) + (length path2)
            || any null [path1, path2]


return []
testLibSuite = $quickCheckAll
